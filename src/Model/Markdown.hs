{-# LANGUAGE RecordWildCards #-}

module Model.Markdown where

import Import

import Text.Pandoc
import Yesod.Markdown
import qualified Data.Text as T

renderMarkdown :: Markdown -> Handler Html
renderMarkdown = renderMarkdownWith return

renderMarkdownWith :: (Text -> Handler Text) -> Markdown -> Handler Html
renderMarkdownWith transform (Markdown markdown) = do
    let ls = T.lines markdown

    ls' <- mapM transform ls

    let eParsedMarkdown = parseMarkdown yesodDefaultReaderOptions
          $ Markdown 
          $ T.unlines ls'

    let parsedMarkdown = case eParsedMarkdown of
            Left pandocError -> error (show pandocError)
            Right md -> md

    return $ 
        writePandoc (yesodDefaultWriterOptions { 
                        writerEmailObfuscation = NoObfuscation
                    })
                    parsedMarkdown

markdownWidget :: Markdown -> Widget
markdownWidget = markdownWidgetWith return

markdownWidgetWith :: (Text -> Handler Text) -> Markdown -> Widget
markdownWidgetWith transform markdown = do
    rendered <- handlerToWidget $ renderMarkdownWith transform markdown
    toWidget rendered
