{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Handlers for CRUD'ing patrons' payment info.
module Handler.PaymentInfo
  ( getPaymentInfoR
  , postPaymentInfoR
  ) where

import Import

import Control.Monad.Logger
import Text.Julius (rawJS)
import Stripe
import Web.Stripe.Customer
import Web.Stripe.Error

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
            let stripeAction = maybe
                    createCustomer'
                    (updateCustomer' . CustomerId)
                    (_userStripeCustomer u)
            (runDB . storeCustomer uid <=< stripePaymentInfoHandler)
                =<< stripeAction u token
            alertSuccess "Payment information stored"
            redirect HomeR
        _ -> do
            alertDanger "There was something wrong with your form submission."
            redirect PaymentInfoR

storeCustomer :: MonadHandler m => Key User -> CustomerId -> SqlPersistT m ()
storeCustomer uid (CustomerId cid) =
    -- FIXME: Store an event
    update uid [UserStripeCustomer =. Just cid]

-- | Wrap over Stripe's native 'createCustomer'
createCustomer' :: User -> PaymentToken -> Handler (Either StripeError Customer)
createCustomer' User{..} (PaymentToken tok) =
    snowstripe (createCustomer -&- Email _userEmail -&- TokenId tok)

-- | Wrap over Stripe's native 'updateCustomer'
updateCustomer' :: CustomerId
                -> User
                -> PaymentToken
                -> Handler (Either StripeError Customer)
updateCustomer' cid User{..} (PaymentToken tok) =
    snowstripe (updateCustomer cid -&- TokenId tok)

-- | The preceding two methods use the same error catching. Pretty bare
-- bones, but at least there will be a log.
stripePaymentInfoHandler :: Either StripeError Customer -> Handler CustomerId
stripePaymentInfoHandler =
    either
        (\er -> do
             alertDanger [shamlet|
                 We at Snowdrift.coop did the best we could, but Stripe
                 experienced a problem. Stripe says, "#{errorMsg er}"
                 ...Maybe try again?
             |]
             $logErrorSH er
             redirect PaymentInfoR)
        (pure . \case
            Customer{..} -> customerId
            -- This case "should never happen" :D But if it does, we can
            -- just ignore it for now.
            -- See also https://github.com/dmjio/stripe/issues/40
            DeletedCustomer{..} -> deletedCustomerId)
