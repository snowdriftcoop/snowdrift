{-# LANGUAGE FlexibleContexts #-}

-- | Handlers for CRUD'ing patrons' payment info.
module Handler.PaymentInfo
  ( getPaymentInfoR
  , postPaymentInfoR
  ) where

import Import

import Control.Monad.Logger (logDebugSH)
import Text.Julius (rawJS)

import Alerts
import Handler.Util

newtype PaymentToken = PaymentToken Text deriving (Show)
newtype Customer = Customer Text deriving (Show)

paymentForm :: Text -> Form PaymentToken
paymentForm tokenId =
    renderDivs
        (PaymentToken <$> areq hiddenField "" {fsId = Just tokenId} Nothing)

getPaymentInfoR :: Handler Html
getPaymentInfoR = do
    email <- _userEmail . entityVal <$> requireAuth
    publishableKey <- appStripePublishableKey . appSettings <$> getYesod
    tokenId <- newIdent
    (paymentWidget, enctype) <- generateFormPost (paymentForm tokenId)
    -- Unfortunately, "page/payment-info" is duplicated in this section of
    -- code. Triplicated, now. :) Fixing this requires reworking the whole
    -- navbarLayout scheme. In turn, that idea lends itself to the idea of
    -- fixing defaultLayout, which is doable now that AppDataTypes exists.
    navbarLayout "page/payment-info" $ do
        addScriptRemote "https://checkout.stripe.com/checkout.js"
        snowdriftTitle "Payment Info"
        paymentFormId <- newIdent
        paymentButtonId <- newIdent
        $(widgetFile "page/payment-info")

postPaymentInfoR :: Handler Html
postPaymentInfoR = do
    ((formResult, _), _) <- runFormPost (paymentForm "")
    case formResult of
        FormSuccess token -> do
            runDB . storeCustomer =<< createCustomer token
            alertSuccess "Payment information stored"
            redirect HomeR
        _ -> do
            alertDanger "There was something wrong with your form submission."
            redirect PaymentInfoR

storeCustomer :: MonadHandler m => Customer -> SqlPersistT m ()
storeCustomer = undefined

createCustomer :: PaymentToken -> Handler Customer
createCustomer = undefined
