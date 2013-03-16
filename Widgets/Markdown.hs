module Widgets.Markdown where

import Import

import Yesod.Markdown

import qualified Data.Text as T


snowdriftMarkdownField :: RenderMessage App FormMessage => Field sub App Markdown
snowdriftMarkdownField = Field
    { fieldParse = parseHelper $ Right . Markdown . T.filter (/= '\r')
    , fieldView  = \theId name attrs val _isReq -> do
            render <- lift getUrlRender
            let tutorial = render MarkdownTutorialR
             in do
                [whamlet|
                    <div .markdown_label>
                        <a href="#{tutorial}">
                            Markdown syntax
                    <div .markdown_wrapper>
                        <textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
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
