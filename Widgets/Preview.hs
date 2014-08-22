module Widgets.Preview where

import Import

previewWidget :: Widget -> Text -> Widget -> Widget
previewWidget form action widget =
    [whamlet|
        <div .alert .alert-danger>
            This is a preview; changes have <em>not</em> been saved!
            Edit and/or submit your posting <a href="#edit-preview">below</a>.

        ^{widget}

        <div .alert .alert-danger>
            This is a preview; your changes have <em>not</em> been saved!

        <form #edit-preview method="POST">
            <button type="submit" name="mode" value="preview">preview
            <button .preview-action-button type="submit" name="mode" value="post">#{action}

            ^{form}

            <button type="submit" name="mode" value="preview">preview
            <button .preview-action-button type="submit" name="mode" value="post">#{action}
    |]
