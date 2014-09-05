module Widgets.Preview where

import Import

previewWidget :: Widget -> Text -> Widget -> Widget
previewWidget form action widget =
    [whamlet|
        <div .alert .alert-danger>
            This is a preview; changes have <em>not</em> been saved!
            Edit and/or submit your posting <a href="#edit-preview">below</a>.

        ^{widget}

        <form #edit-preview method="POST">
            ^{form}
            <button type="submit" name="mode" value="preview">preview
            <button .preview-action-button type="submit" name="mode" value="post">#{action}
    |]
