{-# LANGUAGE RecordWildCards #-}

module Model.Markdown where

import Import

import qualified Data.ByteString.Char8      as BS
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString
import           Text.Pandoc
import           Yesod.Markdown (Markdown (..))

import Model.Discussion


-- TODO: we should probably put together some standard sets of these transforms for use in various places, rather than assembling ad-hoc

fixLinks :: Text -> DiscussionOn -> Text -> Handler Text
fixLinks project' discussion_on line' = do
    render <- getUrlRender

    let Right pattern = compile defaultCompOpt defaultExecOpt $ mconcat
            [ "(\\[[^]]*\\])" -- link
            , "\\("
            , "(" -- _
                , "c/([0-9]+)" -- comment
                , "|"
                , "([a-z]+:)?" -- project
                , "([a-z0-9-]+)" -- page
            , ")"
            , "([a-z0-9/#-]*)?" -- path
            , "\\)"
            ]

        project = encodeUtf8 project'

        expand_match (pre, _, post, matches) = pre <> build_link matches <> parse post

        build_link [link, _, comment, "", "", path] =
            let comment_id :: CommentId
                comment_id = fromMaybe (error "bad comment id") $ fromPathPiece $ decodeUtf8 comment

             in mconcat
                    [ link
                    , "("
                        , let route = encodeUtf8 $ render $ case discussion_on of
                                DiscussionOnProject (Entity _ Project{..}) -> ProjectCommentR projectHandle comment_id
                                DiscussionOnWikiPage _ (Entity _ WikiTarget{..}) -> WikiCommentR project' wikiTargetLanguage wikiTargetTarget comment_id
                                DiscussionOnUser (Entity user_id _) -> UserCommentR user_id comment_id

                           in route <> path
                    , ")"
                    ]

        build_link [link, _, "", proj, page, path] =
            mconcat
                [ link
                , "("
                    , "/p/" <> if BS.null proj then project else BS.init proj
                    , "/w/" <> page <> path
                , ")"
                ]

        build_link [_, content, _, _, _] = error $ "strange match: " <> show content
        build_link _ = error $ "strange match"

        parse str = either error (maybe str expand_match) (regexec pattern str)

        line = encodeUtf8 line'
    return $ decodeUtf8 $ parse line


linkTickets :: Text -> Handler Text
linkTickets line' = do
    let Right pattern = compile defaultCompOpt defaultExecOpt "\\<SD-([0-9][0-9]*)" -- TODO word boundaries?
        getLinkForTicketComment :: TicketId -> Handler (Maybe Text)
        getLinkForTicketComment ticket_id = do
            info <- runDB $ select $ from $ \ (ticket `InnerJoin` comment `LeftOuterJoin` target `LeftOuterJoin` page `LeftOuterJoin` project) -> do
                on_ $ project ?. ProjectId ==. page ?. WikiPageProject
                on_ $ page ?. WikiPageDiscussion ==. just (comment ^. CommentDiscussion)
                on_ $ page ?. WikiPageId ==. target ?. WikiTargetPage
                on_ $ ticket ^. TicketComment ==. comment ^. CommentId
                where_ $ ticket ^. TicketId ==. val ticket_id

                return
                    ( comment ^. CommentId
                    , project ?. ProjectHandle
                    , target ?. WikiTargetTarget
                    )

            case map unwrapValues info of
                [] -> return Nothing
                (Key (PersistInt64 comment_id), Just handle, Just target) : _ -> return $ Just $ mconcat
                    [ "/p/", handle, "/w/", target, "/c/",  T.pack (show comment_id) ]

                _ -> error "Unexpected result for ticket reference"

        parse _   (Left err) = error err
        parse str (Right Nothing) = return str
        parse _   (Right (Just (pre, _, post, [ticket_number]))) = do
            $(logError) $ T.pack $ show $  T.unpack $ decodeUtf8 ticket_number
            maybe_link <- getLinkForTicketComment $ Key $ PersistInt64 $ read $ T.unpack $ decodeUtf8 ticket_number
            rest <- parse post (regexec pattern post)
            return $ mconcat
                [ pre
                , "[SD-" <> ticket_number <> "]"
                , case maybe_link of
                    Nothing -> ""
                    Just link -> mconcat [ "(", encodeUtf8 link, ")" ]
                ] <> rest

        parse _ (Right (Just _)) = error "strange match"

        line = encodeUtf8 line'
     in fmap decodeUtf8 $ parse line (regexec pattern line)

renderMarkdown :: Markdown -> Handler Html
renderMarkdown = renderMarkdownWith return

renderMarkdownWith :: (Text -> Handler Text) -> Markdown -> Handler Html
renderMarkdownWith transform (Markdown markdown) = do
    let ls = T.lines markdown

    ls' <- mapM (transform <=< linkTickets) ls

    return $ writeHtml def
        { writerEmailObfuscation = NoObfuscation
        , writerHtml5 = True
        } $ readMarkdown def $ T.unpack $ T.unlines ls'


markdownWidget :: Markdown -> Widget
markdownWidget = markdownWidgetWith return

markdownWidgetWith :: (Text -> Handler Text) -> Markdown -> Widget
markdownWidgetWith transform markdown = do
    rendered <- handlerToWidget $ renderMarkdownWith transform markdown
    toWidget rendered


fixTests :: [(DiscussionOn, [(Text, Text)])]
fixTests = [minBound .. maxBound] >>= \case
    DiscussionTypeProject -> [(DiscussionOnProject undefined,
            [ ("[test](en/test)", "[test](/p/project/w/en/test)")
            ]
        )]

    DiscussionTypeWikiPage -> [(DiscussionOnWikiPage undefined undefined,
            [ ("[test](en/test)", "[test](/p/project/w/en/test)")
            ]
        )]

    DiscussionTypeUser -> [(DiscussionOnUser undefined,
            [ ]
        )]

testFixLinks :: Handler [(DiscussionOn, Text, Text, Text)]
testFixLinks = do
    fmap (concat . concat) $ forM fixTests $ \ (discussion, examples) -> forM examples $ \ (input, output) -> do
        output' <- fixLinks "project" discussion input
        if output == output'
         then return []
         else return [(discussion, input, output, output')]

