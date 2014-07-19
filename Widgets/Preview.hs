module Widgets.Preview where

import Import

previewWidget :: Widget -> Text -> Widget -> Widget
previewWidget form action widget =
    [whamlet|
        <form method="POST">
            <div .alert .alert-danger>
                This is a preview; changes have <em>not</em> been saved!
                Edit and/or submit your posting <a href="#edit-preview">below</a>.

            ^{widget}

            <input type=submit name=mode value="preview">
            <input .preview-action-button type=submit name=mode value="#{action}">

            <hr>

            ^{form}

            <input type=submit name=mode value="preview">
            <input .preview-action-button type=submit name=mode value="#{action}">
    |]
