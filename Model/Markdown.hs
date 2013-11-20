module Model.Markdown where

import Import

import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString
import Yesod.Markdown (markdownToHtml, Markdown (..))

import qualified Prelude as Partial (init)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import Data.Text.Encoding

fixLinks :: Text -> Text -> Text
fixLinks project' line' =
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
     in decodeUtf8 $ parse line (regexec pattern line)
        
renderMarkdown :: Text -> Markdown -> Html
renderMarkdown project (Markdown markdown) = markdownToHtml $ Markdown $ T.unlines $ map (fixLinks project) $ T.lines markdown

