module Model.Markdown where

import Import

import qualified Data.ByteString.Char8      as BS
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Text.Regex.TDFA
import           Text.Regex.TDFA.ByteString
import           Yesod.Markdown             (markdownToHtml, Markdown (..))


-- TODO: we should probably put together some standard sets of these transforms for use in various places, rather than assembling ad-hoc

fixLinks :: Text -> Text -> Handler Text
fixLinks project' line' = do
    let Right pattern = compile defaultCompOpt defaultExecOpt "(\\[[^]]*\\])\\(([a-z]+:)?([a-z0-9-]+)\\)"
        project = encodeUtf8 project'
        parse _   (Left err) = error err
        parse str (Right Nothing) = str
        parse _   (Right (Just (pre, _, post, [link, proj, page]))) = mconcat
            [ pre
            , link
            , "("
                , "/p/" <> if BS.null proj then project else BS.init proj
                , "/w/" <> page
            , ")"
            ] <> parse post (regexec pattern post)

        parse _ (Right (Just _)) = error "strange match"

        line = encodeUtf8 line'
    return $ decodeUtf8 $ parse line (regexec pattern line)


linkTickets :: Text -> Handler Text
linkTickets line' = do
    let Right pattern = compile defaultCompOpt defaultExecOpt "\\<SD-([0-9][0-9]*)" -- TODO word boundaries?
        getLinkForTicketComment :: TicketId -> Handler (Maybe Text)
        getLinkForTicketComment ticket_id = do
            info <- runDB $ select $ from $ \ (ticket `InnerJoin` comment `LeftOuterJoin` page `LeftOuterJoin` project) -> do
                on_ $ project ?. ProjectId ==. page ?. WikiPageProject
                on_ $ page ?. WikiPageDiscussion ==. just (comment ^. CommentDiscussion)
                on_ $ ticket ^. TicketComment ==. comment ^. CommentId
                where_ $ ticket ^. TicketId ==. val ticket_id

                return
                    ( comment ^. CommentId
                    , project ?. ProjectHandle
                    , page ?. WikiPageTarget
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

    return $ markdownToHtml $ Markdown $ T.unlines ls'


markdownWidget :: Markdown -> Widget
markdownWidget = markdownWidgetWith return

markdownWidgetWith :: (Text -> Handler Text) -> Markdown -> Widget
markdownWidgetWith transform markdown = do
    rendered <- handlerToWidget $ renderMarkdownWith transform markdown
    toWidget rendered

