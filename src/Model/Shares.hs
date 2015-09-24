{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import
import Model.Currency

import System.Random (randomIO)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Text.Julius (rawJS)

pledgeSizes :: [[Int64]]
pledgeSizes =
    [ [1,2,4,8]
    , [1,2,4,8,16]
    , [1,2,3,5,10]
    , [1,2,5,10]
    ]

pledgeListKey :: Text
pledgeListKey = "pledge_list"

pledgeRenderKey :: Text
pledgeRenderKey = "pledge_render"

newtype SharesPurchaseOrder = SharesPurchaseOrder Int64

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
        let shares           = Right . Just . SharesPurchaseOrder
            invalidInteger i = Left $ SomeMessage $ fromString
                "Number of shares must be an integer: " <> i
        in case T.decimal v of
            Right (a, "") -> shares a
            Right (a, bs) ->
                if T.all (== '0') $ T.tail bs
                    then shares a
                    else invalidInteger v
            _ -> invalidInteger v

    view ident name attrs v req = do
        now <- liftIO getCurrentTime
        list <- handlerToWidget get_list
        muser <- handlerToWidget maybeAuthId
        render_key <- handlerToWidget $ runDB $
            insert $
                PledgeFormRendered
                    now
                    (T.pack $ show list)
                    project_id
                    muser

        handlerToWidget $ setSession pledgeRenderKey $ T.pack $ show render_key

        let value = either (const 2) (\(SharesPurchaseOrder s) -> s) v
            hasValue = value `elem` list
            otherValue = if hasValue then "" else show value
            pledgeOptions = zip list $ map (show . millMilray) list

        $(widgetFile "pledge-field")

    get_list = do
        mlist <- lookupSession pledgeListKey
        case mlist of
            Nothing -> do
                r <- liftIO randomIO
                let idx = mod r $ length pledgeSizes
                    sizes = pledgeSizes !! idx

                setSession pledgeListKey $ T.pack $ show sizes

                return sizes

            Just t -> return $ read $ T.unpack t






pledgeForm :: ProjectId -> Form SharesPurchaseOrder
pledgeForm project_id extra = do
    muser <- lift maybeAuthId
    shares <- case muser of
        Nothing -> return 0
        Just user_id ->
            fmap (sum . map unValue) $ lift $ runDB $
                select $ from $ \pledge -> do
                where_ $ pledge ^. PledgeProject ==. val project_id
                    &&. pledge ^. PledgeUser ==. val user_id
                return $ pledge ^. PledgeShares


    (result, pledge_view) <-
        mreq (pledgeField project_id)
             ""
             (if shares > 0 then Just (SharesPurchaseOrder shares) else Nothing)

    let view = $(widgetFile "pledge-form")
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

