{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import


data SharesPurchaseOrder = SharesPurchaseOrder Int64

buySharesForm :: Int64 -> Form SharesPurchaseOrder
buySharesForm shares extra = do
    (pledge_res, pledge_view) <- mreq intField ("" { fsAttrs = [("placeholder", "10"), ("class", "inline_shares")] }) (if shares > 0 then Just shares else Nothing)

    let result = SharesPurchaseOrder <$> pledge_res
        view =  [whamlet|
            #{extra}
            <p>
                <strong>
                    I pledge ^{fvInput pledge_view}&nbsp;shares starting at 1&nbsp;&cent; per month per every 100 other patrons.
            <p>
                &hellip; share value also increases some when any patron pledges extra shares beyond the minimum.
                <br>
                <a href=@{WikiR "snowdrift" "mechanism"}>
                    <em>
                        read the details&hellip;
        |]
    return (result, view)