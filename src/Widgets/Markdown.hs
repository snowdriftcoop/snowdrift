module Widgets.Markdown where

import Import

import qualified Data.Text as T

snowdriftMarkdownField :: (Monad m, HandlerSite m ~ App) => Field m Markdown
snowdriftMarkdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . T.filter (/= '\r')
    , fieldView  = \theId name attrs value _isReq -> do
        [whamlet|
            <div .markdown_label>
                Use
                <a href=# target="_blank">
                    Markdown syntax
            <div .markdown_label>
                Remember to follow the
                <a href=#>
                    Code of Conduct
            <div .markdown_wrapper>
                <textarea id=#{theId} name=#{name} :_isReq:required *{attrs}>#{either id unMarkdown value}
        |]

    , fieldEnctype = UrlEncoded
    }

