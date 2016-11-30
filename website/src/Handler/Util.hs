module Handler.Util
        ( snowdriftTitle
        , snowdriftDashTitle
        , snowstripe
        -- * DELETE handling
        , deleteFromPost
        , handleDelete
        ) where

import Import.NoFoundation

import Crowdmatch
import Data.Text.Titlecase
import Web.Stripe
import Web.Stripe.Error

import AppDataTypes

snowdriftTitle :: MonadWidget m => Text -> m ()
snowdriftTitle t = setTitle $
    toHtml (titlecase t) `mappend` toHtml (" | Snowdrift.coop" :: Text)

snowdriftDashTitle :: MonadWidget m => Text -> Text -> m ()
snowdriftDashTitle x y = snowdriftTitle $ x `mappend` " — " `mappend` y

snowstripe :: StripeI a -> Handler (Either StripeError a)
snowstripe req = do
    conf <- fmap
        (StripeConfig . appStripeSecretKey . appSettings)
        getYesod
    liftIO (runStripe conf req)

-- | The form element that can be inserted to handle a delete.
deleteFromPost
    :: ( site ~ HandlerSite m
       , MonadHandler m
       , RenderMessage site FormMessage)
    => AForm m ()
deleteFromPost =
    areq hiddenField "do-delete" (Just ())

-- | Since forms can only GET or POST, this looks for a 'delete' parameter
-- to mimic DELETE with a POST.
handleDelete
    :: (site ~ HandlerSite m, MonadHandler m, RenderMessage site FormMessage)
    => Text -> m Html -> m Html -> m Html
handleDelete formId deleteHandler postHandler = do
    ((res, _), _) <-
        runFormPost (identifyForm formId (renderDivs deleteFromPost))
    case res of
        FormSuccess _ -> deleteHandler
        _ -> postHandler
