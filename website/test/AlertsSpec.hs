module AlertsSpec (spec) where

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
        malert <- getAlert
        fmap toTypedContent $ defaultLayout $ case malert of
            Just alert -> [whamlet|#{alert}|]
            Nothing -> ""
    go _ _ = Nothing

withAlertSite :: SpecWith (TestApp LiteApp) -> Spec
withAlertSite = before $ pure $ (alertSite, id)

spec :: Spec
spec = withAlertSite $ do
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
