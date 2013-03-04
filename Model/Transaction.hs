
module Model.Transaction where

import Import

import qualified Data.Map as M

import Model.User

renderOtherAccount :: Bool -> Transaction -> M.Map AccountId (Entity User) -> M.Map AccountId (Entity Project) -> Widget
renderOtherAccount is_credit transaction user_accounts project_accounts = do

    let maybe_account_id = if is_credit
                      then transactionDebit transaction
                      else transactionCredit transaction

        maybe_project = maybe Nothing (`M.lookup` project_accounts) maybe_account_id
        maybe_user = maybe Nothing (`M.lookup` user_accounts) maybe_account_id

    toWidget $ case (maybe_project, maybe_user) of
        (Just _, Just _) -> error "account belongs to both a project and a user - this shouldn't happen"
        (Just (Entity _ project), Nothing) ->
            [hamlet|
                <a href="@{ProjectR (projectHandle project)}">
                    #{projectName project}
            |]
        
        (Nothing, Just (Entity user_id user)) ->
            [hamlet|
                <a href="@{UserR user_id}">
                    #{userPrintName (Entity user_id user)}
            |]

        (Nothing, Nothing) ->
            if is_credit
             then
                [hamlet|
                    deposited
                |]
             else
                [hamlet|
                    withdrawn
                |]

