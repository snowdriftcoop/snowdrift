-- | Handler functions that are shared among various different
-- locations Comments may exist.
module Handler.Comment where

import Import

import Model.Comment
import Model.Tag
import Model.User
import View.Comment

import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Network.HTTP.Types.Status (movedPermanently301)

-- | Get the max depth from the "maxdepth" GET param, or 11 (arbitrary) if it doesn't exist.
getMaxDepth :: Handler MaxDepth
getMaxDepth = getMaxDepthDefault 11

-- | Get the max depth from the "maxdepth" GET param, or NoMaxDepth if it doesn't exist.
getMaxDepthNoLimit :: Handler MaxDepth
getMaxDepthNoLimit = maybe NoMaxDepth MaxDepth <$> runInputGet (iopt intField "maxdepth")

-- | Get the max depth from the "maxdepth" GET param, or default to the provided depth.
getMaxDepthDefault :: Int -> Handler MaxDepth
getMaxDepthDefault n = maybe (MaxDepth n) MaxDepth <$> runInputGet (iopt intField "maxdepth")

redirectIfRethreaded :: CommentId -> Handler ()
redirectIfRethreaded comment_id = runDB (fetchCommentRethreadDB comment_id) >>= \case
    Nothing -> return ()
    Just new_comment_id -> redirectWith movedPermanently301 (CommentDirectLinkR new_comment_id)

--------------------------------------------------------------------------------
-- /

getCommentDirectLinkR :: CommentId -> Handler Html
getCommentDirectLinkR comment_id = runDB (fetchCommentWikiPageDB comment_id) >>= \case
    -- comment not on a wiki page? right now, there's nowhere else to check
    -- TODO(mitchell): does this require constant attention?
    Nothing -> notFound
    Just (Entity _ page) -> do
        project <- runYDB (get404 (wikiPageProject page))
        redirect (WikiCommentR (projectHandle project) (wikiPageTarget page) comment_id)

--------------------------------------------------------------------------------
-- /tags

getCommentTagsR :: CommentId -> Handler Html
getCommentTagsR comment_id = do
    muser_id <- maybeAuthId
    tags <- runDB $ (M.! comment_id) <$> (fetchCommentCommentTagsDB comment_id >>= buildAnnotatedCommentTagsDB muser_id)
    defaultLayout $(widgetFile "tags")

--------------------------------------------------------------------------------
-- /tag/#TagId

getCommentTagR :: CommentId -> TagId -> Handler Html
getCommentTagR comment_id tag_id = do
    muser_id <- maybeAuthId
    tags <- runDB $ (M.! comment_id) <$> (fetchCommentTagCommentTagsDB comment_id tag_id >>= buildAnnotatedCommentTagsDB muser_id)
    case tags of
        [] -> error "That tag has not been applied to this comment."
        [tag] -> renderTag tag
        _ -> error "This should never happen."
  where
    renderTag (AnnotatedTag tag _ _ user_votes) = do
        let tag_name = tagName $ entityVal tag
        defaultLayout $(widgetFile "tag")

postCommentTagR :: CommentId -> TagId -> Handler Html
postCommentTagR comment_id tag_id = do
    user_id <- requireAuthId
    direction <- lookupPostParam "direction"

    let delta = case T.unpack <$> direction of
            Just "+" -> 1
            Just "-" -> -1
            Just "\215" -> -1
            Nothing -> error "direction unset"
            Just str -> error $ "unrecognized direction: " ++ str

    runDB $ do
        maybe_comment_tag_entity <- getBy (UniqueCommentTag comment_id tag_id user_id)
        case maybe_comment_tag_entity of
            Nothing -> insert_ (CommentTag comment_id tag_id user_id delta)
            Just (Entity comment_tag_id comment_tag) -> case commentTagCount comment_tag + delta of
                0 -> delete $ from $ \ ct -> where_ $ ct ^. CommentTagId ==. val comment_tag_id
                x -> void $ update $ \ ct -> do
                    set ct [ CommentTagCount =. val x ]
                    where_ $ ct ^. CommentTagId ==. val comment_tag_id

    setUltDestReferer
    redirectUltDest (CommentTagR comment_id tag_id)

--------------------------------------------------------------------------------
-- /tag/apply

postCommentApplyTagR :: CommentId -> Handler Html
postCommentApplyTagR comment_id = do
    Entity user_id user <- requireAuth

    unless (userCanAddTag user) $
        permissionDenied "You must be an established user to add tags"

    ((result_apply, _), _) <- runFormPost (newCommentTagForm [] [])
    case result_apply of
        FormSuccess (mproject_tag_ids, mother_tag_ids) -> do
            let project_tag_ids = fromMaybe [] mproject_tag_ids
                other_tag_ids   = fromMaybe [] mother_tag_ids

            ok <- runDB $ do
                valid_tags <- fetchTagsInDB (project_tag_ids <> other_tag_ids)
                if (null valid_tags)
                    then return False
                    else do
                        void (insertMany $ fmap (\(Entity tag_id _) -> CommentTag comment_id tag_id user_id 1) valid_tags)
                        return True
            unless ok (permissionDenied "Error: Invalid tag ID.")
            redirectUltDest (CommentDirectLinkR comment_id)
        FormMissing -> error "form missing"
        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs

--------------------------------------------------------------------------------
-- /tag/create

postCommentCreateTagR :: CommentId -> Handler Html
postCommentCreateTagR comment_id = do
    Entity user_id user <- requireAuth

    unless (userCanAddTag user) $
        permissionDenied "You must be an established user to add tags"

    ((result_create, _), _) <- runFormPost $ createCommentTagForm
    case result_create of
        FormSuccess tag_name -> do
            tag_exists <- runDB $ getBy (UniqueTag tag_name) >>= \case
                Nothing -> do
                    tag_id <- insert $ Tag tag_name
                    insert_ (CommentTag comment_id tag_id user_id 1)
                    return False
                Just _ -> return True
            when tag_exists (alertDanger "That tag already exists.")
            redirectUltDest (CommentDirectLinkR comment_id)
        FormMissing -> error "form missing"
        FormFailure errs -> error $ T.unpack $ "Form failed: " <> T.intercalate "; " errs
