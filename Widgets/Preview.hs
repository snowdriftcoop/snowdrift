
module Widgets.Preview where

import Import

renderPreview :: Widget -> Text -> Widget -> Widget
renderPreview form action widget =
    [whamlet|
        <form method="POST" style="padding : 0em; margin : 0em">
            <div .row>
                <div .col-md-9>
                    <div .alert .alert-danger>
                        This is a preview; your changes have not been saved!
                        You can edit it below.
                    <input type=submit name=mode value="#{action}">

            ^{widget}

            <div .row>
                <div .col-md-9>
                    <div .alert .alert-danger>
                        This is a preview; your changes have not been saved!
                    <input type=submit name=mode value="preview">
                    <input type=submit name=mode value="#{action}">
                    ^{form}
                    <input type=submit name=mode value="preview">
                    <input type=submit name=mode value="#{action}">
    |]



