module Model.License where

import Import

fetchLicensesDB :: DB [License]
fetchLicensesDB =
    fmap (map entityVal) $
    select $ from $ \(l :: SqlExpr (Entity License)) ->
    return l