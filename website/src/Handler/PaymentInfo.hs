{-# LANGUAGE FlexibleContexts #-}

-- | Handlers for CRUD'ing patrons' payment info.
module Handler.PaymentInfo
  ( getPaymentInfoR
  , postPaymentInfoR
  ) where

import Import

import Text.Julius (rawJS)
import Stripe
import Web.Stripe.Customer

import Alerts
import Handler.Util

newtype PaymentToken = PaymentToken Text deriving (Show)

paymentForm :: Text -> Form PaymentToken
paymentForm tokenId =
    renderDivs
        (PaymentToken <$> areq hiddenField "" {fsId = Just tokenId} Nothing)

getPaymentInfoR :: Handler Html
getPaymentInfoR = do
    email <- _userEmail . entityVal <$> requireAuth
    publishableKey <-
        fmap
            (decodeUtf8 . getStripeKey . appStripePublishableKey . appSettings)
            getYesod
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
    Entity uid u <- requireAuth
    ((formResult, _), _) <- runFormPost (paymentForm "")
    case formResult of
        FormSuccess token -> do
            runDB . storeCustomer uid =<< createCustomer' u token
            alertSuccess "Payment information stored"
            redirect HomeR
        _ -> do
            alertDanger "There was something wrong with your form submission."
            redirect PaymentInfoR

storeCustomer :: MonadHandler m => Key User -> Customer -> SqlPersistT m ()
storeCustomer uid customer =
    -- FIXME: Store an event
    update uid [UserCustomer =. Just cid]
  where (CustomerId cid) = customerId customer

-- | Wrap over Stripe's native 'createCustomer'
createCustomer' :: User -> PaymentToken -> Handler Customer
createCustomer' User{..} (PaymentToken tok) = do
    res <- snowstripe (createCustomer -&- Email _userEmail -&- TokenId tok)
    case res of
        Right c -> pure c
        Left er -> error (show er)
