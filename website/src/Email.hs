{-# LANGUAGE LambdaCase #-}

-- | Email messages for Snowdrift
module Email (snowdriftAuthEmail) where

import ClassyPrelude

import Network.Mail.Mime (Address(..), simpleMail', Mail)
import Text.Shakespeare (RenderUrl)
import Text.Shakespeare.Text (textFile, renderTextUrl, TextUrl)
import Yesod.Core (Route)

import AppDataTypes
import AuthSite (AuthMailMessage(..), AuthToken(..), AuthEmail(..))
import AuthSiteDataTypes -- Auth routes

snowdriftAuthEmail :: RenderUrl (Route App) -> AuthEmail -> AuthMailMessage -> Mail
snowdriftAuthEmail r AuthEmail{..} amsg = simpleMail' to from subject body
  where
    to = Address Nothing fromAuth
    from = Address (Just "Snowdrift.coop Auth System") "no-reply@snowdrift.coop"
    subject = msgSubject amsg
    body = renderTextUrl r (msgBody amsg)

-- | Choose the email subject
msgSubject :: AuthMailMessage -> Text
msgSubject = \case
    VerifyUserCreation _ -> "Verify your Snowdrift.coop account"
    VerifyPassReset _ -> "Reset your Snowdrift.coop passphrase"
    BadUserCreation -> "Creating a Snowdrift.coop account"
    BadPassReset -> "Resetting your Snowdrift.coop passphrase"

-- | Build the body of the message.
msgBody :: AuthMailMessage -> TextUrl (Route App)
msgBody = \case
    VerifyUserCreation tok -> do
        let authToken = fromAuthToken tok
        $(textFile "templates/email/verify-user-creation.md")
    VerifyPassReset tok -> do
        let authToken = fromAuthToken tok
        $(textFile "templates/email/verify-passphrase-reset.md")
    BadUserCreation -> $(textFile "templates/email/bad-user-creation.md")
    BadPassReset -> $(textFile "templates/email/bad-pass-reset.md")
