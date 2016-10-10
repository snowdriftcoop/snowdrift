-- | Handlers for CRUD'ing patrons' payment info.
module Handler.PaymentInfo
  ( getPaymentInfoR
  , postPaymentInfoR
  , deletePaymentInfoR
  ) where

import Import

import Control.Monad.Logger
import Control.Lens
import Data.Maybe
import Text.Julius (rawJS)
import Web.Stripe
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
    Entity uid User{..} <- requireAuth
    Entity pid Patron{..} <- runDB (fetchUserPatron uid)
    deletePaymentInfoWidget <- fst <$> generateFormPost deletePaymentInfoForm
    publishableKey <-
        fmap
            (decodeUtf8 . getStripeKey . appStripePublishableKey . appSettings)
            getYesod
    tokenId <- newIdent
    (paymentWidget, enctype) <-
        generateFormPost
            (identifyForm modFormId (paymentForm tokenId))
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

delFormId, modFormId :: Text
delFormId = "delete-payment-info"
modFormId = "modify-payment-info"

deletePaymentInfoForm :: Form Bool
deletePaymentInfoForm =
    identifyForm delFormId (renderDivsNoLabels deleteFromPost)

postPaymentInfoR :: Handler Html
postPaymentInfoR = handleDelete delFormId deletePaymentInfoR $ do
    Entity uid User{..} <- requireAuth
    ((formResult, _), _) <-
        runFormPost (identifyForm modFormId (paymentForm ""))
    case formResult of
        FormSuccess token -> do
            Entity pid Patron{..} <- runDB (fetchUserPatron uid)
            let stripeAction = maybe
                    (createCustomer' _userEmail)
                    updateCustomer'
                    _patronStripeCustomer
            (runDB . storeCustomer uid
                <=< stripeCustomerHandler
                <=< stripeAction)
                token
            alertSuccess "Payment information stored"
            redirect HomeR
        _ -> do
            alertDanger "There was something wrong with your form submission."
            redirect PaymentInfoR

deletePaymentInfoR :: Handler Html
deletePaymentInfoR = do
    Entity uid User {..} <- requireAuth
    Entity pid Patron{..} <- runDB (fetchUserPatron uid)
    maybe
        (alertWarning "There was no payment information on file for you!")
        (stripeDeletionHandler pid <=< snowstripe . deleteCustomer)
        _patronStripeCustomer
    redirect DashboardR
  where
    stripeDeletionHandler pid =
        either
            stripeError
            (const $ do
                runDB (deleteFrom pid)
                alertSuccess "Your payment information has been cleared.")
    deleteFrom pid = do
        now <- liftIO getCurrentTime
        insert_ (PledgeHistory pid now DeletePledge)
        update
            pid
                [PatronStripeCustomer =. Nothing, PatronPledgeSince =. Nothing]

storeCustomer :: MonadHandler m => Key User -> CustomerId -> SqlPersistT m ()
storeCustomer uid cid = do
    -- FIXME: Store an event
    now <- liftIO getCurrentTime
    void $ upsert
        (Patron uid now (Just cid) 0 Nothing)
        [PatronStripeCustomer =. Just cid]

-- | Wrap over Stripe's native 'createCustomer'
createCustomer' :: Text -> PaymentToken -> Handler (Either StripeError Customer)
createCustomer' email (PaymentToken tok) =
    snowstripe (createCustomer -&- Email email -&- TokenId tok)

-- | Wrap over Stripe's native 'updateCustomer'
updateCustomer' :: CustomerId
                -> PaymentToken
                -> Handler (Either StripeError Customer)
updateCustomer' cid (PaymentToken tok) =
    snowstripe (updateCustomer cid -&- TokenId tok)

-- | The preceding methods use the same error catching. Pretty bare bones,
-- but at least there will be a log.
stripeCustomerHandler :: Either StripeError Customer -> Handler CustomerId
stripeCustomerHandler =
    either
        stripeError
        (pure . \case
            Customer{..} -> customerId
            -- This case "should never happen" :D But if it does, we can
            -- just ignore it for now.
            -- See also https://github.com/dmjio/stripe/issues/40
            DeletedCustomer{..} -> deletedCustomerId)

stripeError :: StripeError -> Handler a
stripeError er = do
    alertDanger [shamlet|
        <p>
            We at Snowdrift.coop did the best we could, but Stripe
            experienced a problem. Stripe says, "#{errorMsg er}"
        <p>
            Your payment info was <em>not</em> modified. Maybe try again?
    |]
    $logErrorSH er
    redirect PaymentInfoR
