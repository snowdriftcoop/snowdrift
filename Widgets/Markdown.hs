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
                    <div .markdown_wrapper>
                        <textarea id="#{theId}" name="#{name}" *{attrs}>#{either id unMarkdown val}
                    <div .markdown_label>
                        <a href="#{tutorial}">
                            Markdown syntax
                |]

                toWidget $ [cassius|
                            .markdown_field
                                padding-bottom : 1.2em

                            .markdown_label
                                position : relative
                                left : 0.8em
                                top : -2em
                                font-size : x-small
                                padding : 0
                                margin : 0
                           |]
    , fieldEnctype = UrlEncoded
    }
