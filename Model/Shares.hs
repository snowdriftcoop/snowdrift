{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import

import System.Random (randomIO)
import qualified Data.Text as T
import qualified Data.Text.Read as T

pledgeSizes :: [[Int64]]
pledgeSizes =
    [ [1,2,4,8,16]
    , [1,2,3,5,10]
    , [1,2,5,10]
    ]

pledgeListKey :: Text
pledgeListKey = "pledge_list"

pledgeRenderKey :: Text
pledgeRenderKey = "pledge_render"

data SharesPurchaseOrder = SharesPurchaseOrder Int64

pledgeField :: ProjectId -> Field Handler SharesPurchaseOrder
pledgeField project_id = Field
    { fieldParse = parse
    , fieldView = view
    , fieldEnctype = UrlEncoded
    }
  where
    parse [] _ = return $ Left $ SomeMessage MsgValueRequired
    parse (x:_) _
        | "-other" `T.isSuffixOf` x = do
            mv <- lookupGetParam x
            case mv of
                Nothing -> return $ Left $ SomeMessage MsgValueRequired
                Just v -> return $ parseValue v
            
        | otherwise = return $ parseValue x

    parseValue v = 
        case T.decimal v of
            Right (a, "") -> Right $ Just $ SharesPurchaseOrder a
            _ -> Left $ SomeMessage $ MsgInvalidInteger v
            
    view ident name attrs v req = do
        now <- liftIO getCurrentTime
        list <- handlerToWidget get_list
        muser <- handlerToWidget maybeAuthId
        render_key <- handlerToWidget $ runDB $ insert $ PledgeFormRendered now (T.pack $ show list) project_id muser

        handlerToWidget $ setSession pledgeRenderKey $ T.pack $ show render_key

        let value = either (const 2) (\ (SharesPurchaseOrder s) -> s) v
            hasValue = any (== value) list
            otherValue = if hasValue then "" else show value

        [whamlet|
            $newline never
            <fieldset>
                $forall amount <- list
                    <input id="#{ident}-#{amount}" .radio-inline name="#{name}" *{attrs} type="radio" :req:required value="#{amount}" :amount == value:checked>
                    #{amount}
                    
                <div>
                    <input id="#{ident}-other" .radio-inline name="#{name}" *{attrs} type="radio" :req:required value="#{name}-other" :not hasValue:checked>other:&nbsp;
                    <input id="#{ident}-other-val" .form-inline style="width : 2.5em; text-align : center" name="#{name}-other" *{attrs} type="text" value="#{otherValue}">
        |]


    get_list = do
        r <- liftIO randomIO
        let idx = mod r $ length pledgeSizes
            sizes = pledgeSizes !! idx

        setSession pledgeListKey $ T.pack $ show sizes

        return sizes



    

pledgeForm :: ProjectId -> Form SharesPurchaseOrder
pledgeForm project_id extra = do
    muser <- lift maybeAuthId
    shares <- case muser of
        Nothing -> return 0
        Just user_id -> do
            fmap (sum . map unValue) $ lift $ runDB $ select $ from $ \ pledge -> do
                where_ $ pledge ^. PledgeProject ==. val project_id
                    &&. pledge ^. PledgeUser ==. val user_id
                return $ pledge ^. PledgeShares

    
    (result, pledge_view) <- mreq (pledgeField project_id) "" (if shares > 0 then Just (SharesPurchaseOrder shares) else Nothing)

    let view = [whamlet|
            #{extra}
            <div .text-center>
                <h3 style="margin-top:0">
<<<<<<< HEAD
                    How many shares will you pledge?

=======
                    You pledge:
>>>>>>> 5e2cfe36d5037737e70fff28b97ede708effaf4c
                <strong>
                    ^{fvInput pledge_view}
            <p>
<<<<<<< HEAD
                Share value is based on 0.1&cent; times the number of other patrons,
                with additional <i>partial</i> matching for any patron who pledges extra shares.
=======
                Share value starts at 0.1&cent; times the number of other patrons,
                but extra shares from any one patron also get <i>partial</i> matching.
>>>>>>> fixed look of the new radio-style pledge form
                <a href=@{WikiR "snowdrift" "mechanism"}>
                    <em> Read the details&hellip;
        |]
    return (result, view)

-- |previewPledgeForm is used for previewing a project page when editing
previewPledgeForm :: Form SharesPurchaseOrder
previewPledgeForm extra = do
    let view = [whamlet|
            #{extra}
            <p>
                <strong>
                    [Pledge form…]
        |]

    return (FormSuccess $ SharesPurchaseOrder 0, view)

