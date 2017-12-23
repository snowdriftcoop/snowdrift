module AlertsSpec (spec_alerts) where

import TestImport
import Alerts
import Yesod.Core

alertSite :: LiteApp
alertSite = LiteApp go
  where
    go _ ["set"] = Just $ do
        alertWarning "Hey bub"
        pure $ toTypedContent ("" :: Html)
    go _ ["get"] = Just $ do
        msgs <- getMessages
        toTypedContent <$> defaultLayout
            [whamlet|
                $forall (level, msg) <- msgs
                    <div .alert .alert-#{level}>
                        #{msg}
            |]
    go _ _ = Nothing

withAlertSite :: SpecWith (TestApp LiteApp) -> Spec
withAlertSite = before $ pure (alertSite, id)

spec_alerts :: Spec
spec_alerts = withAlertSite $
    it "adds an alert" $ do
        get getR
        htmlCount ".alert" 0
        get setR
        get getR
        htmlCount ".alert" 1
        htmlAnyContain "div.alert.alert-warning" "bub"
  where
    getR, setR :: Text
    getR = "get"
    setR = "set"
