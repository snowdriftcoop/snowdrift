{-# LANGUAGE FlexibleContexts #-}

-- | Handlers for CRUD'ing patrons' payment info.
module Handler.PaymentInfo
  ( getPaymentInfoR
  , postPaymentInfoR
  ) where

import Import

import Text.Julius (rawJS)

import Handler.Util

newtype PaymentInfo = PaymentInfo Text deriving (Show)

paymentForm :: Text -> Form PaymentInfo
paymentForm tokenId =
    renderDivs
        (PaymentInfo <$> areq hiddenField "" {fsId = Just tokenId} Nothing)

getPaymentInfoR :: Handler Html
getPaymentInfoR = do
    email <- _userEmail . entityVal <$> requireAuth
    publishableKey <- appStripePublishableKey . appSettings <$> getYesod
    tokenId <- newIdent
    (paymentWidget, enctype) <- generateFormPost (paymentForm tokenId)
    navbarLayout "page/payment-info" $ do
        addScriptRemote "https://checkout.stripe.com/checkout.js"
        snowdriftTitle "Payment Info"
        paymentFormId <- newIdent
        paymentButtonId <- newIdent
        $(widgetFile "page/payment-info")

postPaymentInfoR :: Handler Html
postPaymentInfoR = defaultLayout [whamlet|PaymentInfo|]
