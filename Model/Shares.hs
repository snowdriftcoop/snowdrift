{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import


data SharesPurchaseOrder = SharesPurchaseOrder Int64

buySharesForm :: Int64 -> Form SharesPurchaseOrder
buySharesForm shares extra = do
    (pledge_res, pledge_view) <- mreq intField ("" { fsAttrs = [("placeholder", "shares"), ("class", "inline_shares")] }) (Just shares)

    let result = SharesPurchaseOrder <$> pledge_res
        view =  [whamlet|
            #{extra}
            <p>
                <strong>
                    I pledge ^{fvInput pledge_view}&nbsp;&cent;/month for every 100 others who join me in pledging!
            <p>
                &hellip; and I'll add just a little extra if others increase their base pledge beyond the minimum, knowing that they will do the same if I increase my pledge level.
            <p>
                <a href=@{WikiR "mechanism"}>
                    <em>
                        read the details&hellip;
        |]
    return (result, view)

