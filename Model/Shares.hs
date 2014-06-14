{-# LANGUAGE TupleSections #-}

module Model.Shares where

import Import


data SharesPurchaseOrder = SharesPurchaseOrder Int64

pledgeForm :: Int64 -> Form SharesPurchaseOrder
pledgeForm shares extra = do
    (pledge_res, pledge_view) <- mreq intField ("" { fsAttrs = [("placeholder", "1"), ("class", "inline_shares")] }) (if shares > 0 then Just shares else Nothing)

    let result = SharesPurchaseOrder <$> pledge_res
        view = [whamlet|
            #{extra}
            <div .text-center>
                <h3 style="margin-top:0">
                    Your pledge:
                <strong>
                    shares:&nbsp;^{fvInput pledge_view}
            <p>
                Share value is based on 0.1&cent; times the number of other patrons.
                When a patron pledges extra shares, those get <i>partial</i> matching as well.
                <a href=@{WikiR "snowdrift" "mechanism"}>
                    <em> Read the details&hellip;
        |]
    return (result, view)

-- |previewPledgeForm is used for previewing a project page when editing
previewPledgeForm :: Int64 -> Form SharesPurchaseOrder
previewPledgeForm _ extra = do
    let view = [whamlet|
            #{extra}
            <p>
                <strong>
                    [Pledge form…]
        |]

    return (FormSuccess $ SharesPurchaseOrder 0, view)

