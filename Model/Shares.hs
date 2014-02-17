{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import


data SharesPurchaseOrder = SharesPurchaseOrder Int64

buySharesForm :: Int64 -> Form SharesPurchaseOrder
buySharesForm shares extra = do
    (pledge_res, pledge_view) <- mreq intField ("" { fsAttrs = [("placeholder", "10"), ("class", "inline_shares")] }) (if shares > 0 then Just shares else Nothing)

    let result = SharesPurchaseOrder <$> pledge_res
        view = [whamlet|
            #{extra}
            <p .text-center>
                <strong>
                    I pledge ^{fvInput pledge_view}&nbsp;shares.
            <p>
                Share value is based at 1&cent; per month per every 100 other patrons
                and also increases somewhat when any patron pledges multiple shares.
                <a href=@{WikiR "snowdrift" "mechanism"}>
                    <em> Read the details&hellip;
        |]
    return (result, view)

mockBuySharesForm :: Int64 -> Form SharesPurchaseOrder
mockBuySharesForm _ extra = do
    let view = [whamlet|
            #{extra}
            <p>
                <strong>
                    I pledge ... shares.
            <p>
                Share value is based at 1&cent; per month per every 100 other patrons
                and also increases somewhat when any patron pledges multiple shares.
                <a href=@{WikiR "snowdrift" "mechanism"}>
                    <em> Read the details&hellip;
        |]

    return (FormSuccess $ SharesPurchaseOrder 0, view)
