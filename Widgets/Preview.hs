module Widgets.Preview ( previewWidget
                       , previewWidgetWithAgreement ) where

import Import

-- |A previewWidget where you can specify a license agreement alert.
-- This is useful for the editing of wiki pages, since you can make
-- users agree with a license.
previewWidgetWithAgreement :: Text -> Widget -> Text -> Widget -> Widget
previewWidgetWithAgreement agreement form action widget
    = previewWidget' (Just agreement) form action widget

previewWidget' :: Maybe Text -> Widget -> Text -> Widget -> Widget
previewWidget' maybeAgreement form action widget =
    [whamlet|
        <div .alert .alert-danger>
            This is a preview; changes have <em>not</em> been saved!
            Edit and/or submit your posting <a href="#edit-preview">below</a>.

        ^{widget}

        <form #edit-preview method="POST">
            ^{form}
            $maybe agreement <- maybeAgreement
                <div .notice>#{agreement}
            <button type="submit" name="mode" value="preview">preview
            <button .preview-action-button type="submit" name="mode" value="post">#{action}
    |]

previewWidget :: Widget -> Text -> Widget -> Widget
previewWidget form action widget = previewWidget' Nothing form action widget
