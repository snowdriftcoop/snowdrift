
module Widgets.Markdown where

import Import

import Yesod.Markdown


snowdriftMarkdownField :: (RenderMessage App FormMessage) => Field App App Markdown
snowdriftMarkdownField = do
    let view_func ident name attr result req = do
            render <- lift getUrlRender
            let view :: Widget
                view = (fieldView markdownField) ident name (("class", "markdown_field"):attr) result req
                tutorial = render MarkdownTutorialR
             in do
                [whamlet|
                    <div .markdown_wrapper>
                        ^{view}
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

     in markdownField { fieldView = view_func }

