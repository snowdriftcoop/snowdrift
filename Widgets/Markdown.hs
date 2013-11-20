module Widgets.Markdown where

import Import

import Yesod.Markdown

import qualified Data.Text as T


snowdriftMarkdownField :: (Monad m, HandlerSite m ~ App) => Field m Markdown
snowdriftMarkdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . T.filter (/= '\r')
    , fieldView  = \theId name attrs value _isReq -> do
            render <- getUrlRender
            let tutorial = render MarkdownTutorialR
             in do
                [whamlet|
                    <div .markdown_label>
                        <a href="#{tutorial}" target="_blank">
                            Markdown syntax
                    <div .markdown_wrapper>
                        <textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown value}
                |]

                toWidget $ [cassius|
                            .markdown_wrapper
                                padding-bottom : 1.2em

                            .markdown_label
                                font-size : x-small
                                padding : 0
                                margin-top : -0.7em
                                margin-bottom : -0.4em
                           |]
    , fieldEnctype = UrlEncoded
    }
