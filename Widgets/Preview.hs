
module Widgets.Preview where

import Import

renderPreview :: Widget -> Text -> Widget -> Widget
renderPreview form action widget =
    [whamlet|
        <form method="POST">
            <div .alert .alert-danger>
                This is a preview; your changes have <em>not</em> been saved!
                Edit your posting <a href="#edit-preview">below</a>.
            <input type=submit name=mode value="#{action}">

            ^{widget}

            <div #edit-preview .alert .alert-danger>
                This is a preview; your changes have <em>not</em> been saved!
            <input type=submit name=mode value="preview">
            <input type=submit name=mode value="#{action}">
            ^{form}
            <input type=submit name=mode value="preview">
            <input type=submit name=mode value="#{action}">
    |]
