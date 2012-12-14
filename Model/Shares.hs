
module Model.Shares where

import Import


data SharesPurchaseOrder = SharesPurchaseOrder Int64

buySharesForm :: Int64 -> Form SharesPurchaseOrder
buySharesForm 0 = renderDivs $ SharesPurchaseOrder
    <$> areq intField ("Shares to pledge:" { fsAttrs = [("placeholder", "shares")] }) Nothing
buySharesForm shares = renderDivs $ SharesPurchaseOrder
    <$> areq intField ("Adjust pledge:" { fsAttrs = [("placeholder", "shares")] }) (Just shares)

