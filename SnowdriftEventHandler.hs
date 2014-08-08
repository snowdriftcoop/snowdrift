module SnowdriftEventHandler
    ( snowdriftEventHandlers
    ) where

import Import

import           Model.Discussion
import           Model.Message
import           Model.Project
import           Model.User

import           Blaze.ByteString.Builder (toLazyByteString)
import           Control.Monad.Reader
import           Data.Maybe               (fromJust)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Database.Persist
import           Yesod                    (renderRoute)
import           Yesod.Markdown

-- Add more event handlers here.
snowdriftEventHandlers :: [SnowdriftEvent -> Daemon ()]
snowdriftEventHandlers =
    [ messageEventHandler
    , eventInserterHandler
    ]

-- | Handler in charge of sending Messages to interested parties.
messageEventHandler :: SnowdriftEvent -> Daemon ()
-- Notify the comment's parent's poster that their comment has been replied to (per their preferences).
messageEventHandler (ECommentPosted comment_id comment) = case commentParent comment of
    Nothing -> return ()
    Just parent_comment_id -> do
        (parent_user_id, delivery) <- runDB $ do
            parent_user_id <- commentUser <$> Database.Persist.getJust parent_comment_id
            delivery <- fetchUserMessagePrefDB parent_user_id MessageReply
            return (parent_user_id, delivery)
        -- Any non-Nothing delivery implies an internal Message should be sent.
        when (isJust delivery) $ do
            app <- ask
            let parent_comment_route = renderRoute' (CommentDirectLinkR parent_comment_id) app
                reply_comment_route  = renderRoute' (CommentDirectLinkR comment_id)        app

            let content = mconcat
                  [ "Someone replied to [your comment]("
                  , Markdown parent_comment_route
                  , ")! You can view the reply [here]("
                  , Markdown reply_comment_route
                  , ")."
                  , ""
                  , "*You can filter these messages by adjusting the settings in your profile.*"
                  ]
            void $ runSDB (sendNotificationMessageDB MessageReply parent_user_id content)
-- Notify all moderators of the project the comment was posted on.
messageEventHandler (ECommentPending comment_id comment) = do
    app <- ask
    runSDB $ lift (fetchDiscussionProjectDB (commentDiscussion comment)) >>= \case
        Nothing -> return () -- Comment wasn't on a project, somehow? I guess do nothing.
        Just project_id -> do
            project <- getJust project_id

            let content = mconcat
                  [ "An unapproved comment has been posted on a "
                  , Markdown (projectName project)
                  , " page. Please view it [here]("
                  , Markdown (renderRoute' (CommentDirectLinkR comment_id) app)
                  , ")."
                  ]

            lift (fetchProjectModeratorsDB project_id) >>=
              mapM_ (\user_id -> sendNotificationMessageDB MessageDirect user_id content)
messageEventHandler _ = return ()

-- | Handler in charge of inserting events (stripped down) into a separate table for each type.
eventInserterHandler :: SnowdriftEvent -> Daemon ()
-- If an unapproved comment is sent as an ECommentPosted event, bad things will happen (fromJust).
eventInserterHandler (ECommentPosted comment_id Comment{..})                         = runDB (insert_ (EventCommentPosted (fromJust commentModeratedTs) comment_id))
eventInserterHandler (ECommentPending comment_id Comment{..})                        = runDB (insert_ (EventCommentPending commentCreatedTs comment_id))
eventInserterHandler (EMessageSent message_id Message{..})                           = runDB (insert_ (EventMessageSent messageCreatedTs message_id))
eventInserterHandler (EWikiPage wiki_page_id WikiPage{..})                           = runDB (insert_ (EventWikiPage wikiPageCreatedTs wiki_page_id))
eventInserterHandler (EWikiEdit wiki_edit_id WikiEdit{..})                           = runDB (insert_ (EventWikiEdit wikiEditTs wiki_edit_id))
eventInserterHandler (ENewPledge shares_pledged_id SharesPledged{..})                = runDB (insert_ (EventNewPledge sharesPledgedTs shares_pledged_id))
eventInserterHandler (EUpdatedPledge old_shares shares_pledged_id SharesPledged{..}) = runDB (insert_ (EventUpdatedPledge sharesPledgedTs old_shares shares_pledged_id))
eventInserterHandler (EDeletedPledge ts user_id project_id shares)                   = runDB (insert_ (EventDeletedPledge ts user_id project_id shares))

renderRoute' :: Route App -> App -> Text
renderRoute' route app =
    let (path_pieces, query_params) = renderRoute route
    -- converting a lazy ByteString to a strict Text... ridiculous!
    -- why does joinPath return a ByteString??
    in TL.toStrict $ TLE.decodeUtf8 $ toLazyByteString (joinPath app "" path_pieces query_params)
