{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module MechanismTest (mechanismSpecs) where

import TestImport hiding (get)
import Model.Currency (Milray (..))
import Model.Project (projectComputeShareValue, fetchProjectSharesDB)

import Control.Applicative ((<$>))
import Database.Esqueleto hiding (delete)
import Data.Int (Int64)
import Data.List (delete)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Test.QuickCheck

mechanismSpecs :: Spec
mechanismSpecs = ydescribe "mechanism" $ do
    testFormula
    testPledges

testFormula :: Spec
testFormula = yit "formula" $ do
    -- See the "Examples" section at
    -- https://snowdrift.coop/p/snowdrift/w/en/formula
    errorUnlessExpected "100 patrons @ 1 share = 10¢"
        (Milray 1000) $ projectComputeShareValue $ replicate 100 1
    -- The number of shares is not taken into account when the project
    -- share value is computed.
    errorUnlessExpected "100 patrons @ 4 shares = 10¢"
        (Milray 1000) $ projectComputeShareValue $ replicate 100 4
    errorUnlessExpected "200 patrons @ 4 shares = 20¢"
        (Milray 2000) $ projectComputeShareValue $ replicate 200 4
    errorUnlessExpected "200 patrons @ 4 shares + 1 patron @ 1 share = 20.1¢"
        (Milray 2010) $ projectComputeShareValue $ 1 : replicate 200 4
    errorUnlessExpected "no shares = 0¢"
        (Milray 0) $ projectComputeShareValue []

    testProperty "any number of 0 shares returns 0" $
        \n -> (projectComputeShareValue $ replicate n 0) === (Milray 0)
    testProperty ("any number of pledges is the same " <>
                  "as the product of that number and 0.1¢") $
        \(Positive n) (Positive m) ->
              (projectComputeShareValue $ replicate n m) ===
              (Milray $ fromIntegral n * 10)

testPledges :: Spec
testPledges = ydescribe "pledges" $ do
    -- Pledge the suggested number of shares plus other amounts.
    ydescribe "single user" $ do
        yit "add $100 to users' accounts" $ do
            -- It's not possible to pledge without money, so load
            -- $100 (the allowed maximum amount).
            loginAs Mary
            userId Mary >>= flip loadFunds 100
            loginAs Bob
            userId Bob >>= flip loadFunds 100
            loginAs Sue
            userId Sue >>= flip loadFunds 100

        -- Test that increasing shares works.
        singleUser 1
        singleUser 2
        singleUser 3
        -- Test that shares can be dropped.
        singleUser 0
        singleUser 5
        singleUser 8
        -- Test that decreasing works, too.
        singleUser 4
        singleUser 16
        singleUser 10
        -- Drop again.
        singleUser 0

    ydescribe "three users" $ do
        -- Test that increasing shares works.
        threeUsersOneShare
        threeUsersTwoShares
        threeUsersThreeShares
        threeUsersFiveShares
        -- Test that shares can be dropped.
        threeUsersZeroShares
        threeUsersEightShares
        -- Test that decreasing works, too.
        threeUsersFourShares
        -- Drop again.
        threeUsersZeroShares

showShares :: Int64 -> String
showShares n = show n <> " " <> Text.unpack (plural n "share" "shares")

singleUser :: Int64 -> Spec
singleUser n =
    yit (showShares n) $ do
        snowdrift_id <- snowdriftId
        testPledge Mary snowdrift_id n

threeUsers :: Int64 -> Int64 -> Int64 -> Spec
threeUsers x y z =
    yit (showShares x <> ", " <>
         showShares y <> ", " <>
         showShares z) $ do
        snowdrift_id <- snowdriftId
        testPledge Mary snowdrift_id x
        testPledge Bob  snowdrift_id y
        testPledge Sue  snowdrift_id z

threeUsersOneShare :: Spec
threeUsersOneShare = threeUsers 1 1 1

threeUsersTwoShares :: Spec
threeUsersTwoShares = do
    threeUsers 2 1 1
    threeUsers 2 2 1
    threeUsers 2 2 2

threeUsersThreeShares :: Spec
threeUsersThreeShares = do
    threeUsers 3 1 1
    threeUsers 3 2 1
    threeUsers 3 3 1
    threeUsers 3 3 2
    threeUsers 3 3 3

threeUsersFourShares :: Spec
threeUsersFourShares = do
    threeUsers 4 1 1
    threeUsers 4 2 1
    threeUsers 4 3 1
    threeUsers 4 4 1
    threeUsers 4 4 2
    threeUsers 4 4 3
    threeUsers 4 4 4

threeUsersFiveShares :: Spec
threeUsersFiveShares = do
    threeUsers 5 1 1
    threeUsers 5 2 1
    threeUsers 5 3 1
    threeUsers 5 4 1
    threeUsers 5 5 1
    threeUsers 5 5 2
    threeUsers 5 5 3
    threeUsers 5 5 4
    threeUsers 5 5 5

threeUsersEightShares :: Spec
threeUsersEightShares = do
    threeUsers 8 1 1
    threeUsers 8 2 1
    threeUsers 8 3 1
    threeUsers 8 4 1
    threeUsers 8 5 1
    threeUsers 8 6 1
    threeUsers 8 7 1
    threeUsers 8 8 1
    threeUsers 8 8 2
    threeUsers 8 8 3
    threeUsers 8 8 4
    threeUsers 8 8 5
    threeUsers 8 8 6
    threeUsers 8 8 7
    threeUsers 8 8 8

threeUsersZeroShares :: Spec
threeUsersZeroShares = do
    threeUsers 0 4 4
    threeUsers 0 3 4
    threeUsers 0 2 4
    threeUsers 0 1 4
    threeUsers 0 0 4
    threeUsers 0 0 3
    threeUsers 0 0 2
    threeUsers 0 0 1
    threeUsers 0 0 0

projectAccountAndUserAccount :: ProjectId -> UserId
                             -> SqlPersistM (Account, Account)
projectAccountAndUserAccount project_id user_id = do
    project <- getOrError project_id
    user    <- getOrError user_id
    project_account <- getOrError $ projectAccount project
    user_account    <- getOrError $ userAccount user
    return (project_account, user_account)

(*$) :: Int64 -> Milray -> Milray
(*$) x (Milray y) = Milray $ x * y

testPledge :: NamedUser -> ProjectId -> Int64 -> Example ()
testPledge named_user project_id shares = do
    user_id <- userId named_user

    -- Get the values before the actual pledge and payout.
    ( project_name, project_account, user_account, old_shares,
      new_project_shares, expected_share_value ) <- testDB $ do
        old_project_shares <- fetchProjectSharesDB project_id
        old_user_shares    <- getBy (UniquePledge user_id project_id) >>= \case
            Nothing -> return 0
            Just v  -> return $ pledgeFundedShares $ entityVal v

        let new_project_shares =
                -- 'projectComputeShareValue' returns 0 if any of the
                -- values passed to it is 0, so it needs to be guarded
                -- against.
                (if shares == 0 then id else (shares :)) $
                    delete old_user_shares old_project_shares

        (project_account, user_account) <-
            projectAccountAndUserAccount project_id user_id

        project <- getOrError project_id

        return ( projectName project
               , project_account
               , user_account
               , old_user_shares
               , new_project_shares
               , projectComputeShareValue new_project_shares )

    let expected_user_balance =
            accountBalance user_account - (shares *$ expected_share_value)
        expected_project_balance =
            accountBalance project_account +
                sum ((*$ expected_share_value) <$> new_project_shares)

    -- Pledge.
    loginAs named_user
    if old_shares == shares && not (shares == 0)
        then pledgeFail project_id shares
        else pledge project_id shares

    -- Transfer funds to projects.
    now <- liftIO getCurrentTime
    -- It's necessary to insert into the 'payday' table before running
    -- 'SnowdriftProcessPayments'.
    testDB $ insert_ $ Payday now
    processPayments (Text.unpack $ "paid to " <> project_name) $ do
        -- Get the values after the payout.
        (project_account', user_account') <-
            testDB $ projectAccountAndUserAccount project_id user_id

        -- Check the project's share value.
        actual_share_value <-
            testDB $ fetchProjectSharesDB project_id >>=
                return . projectComputeShareValue

        errorUnlessExpected "project share value"
            expected_share_value
            actual_share_value

        -- Check the user's balance.
        errorUnlessExpected "user balance"
            expected_user_balance $
            accountBalance user_account'

        -- Check the project's balance.
        errorUnlessExpected "project balance"
            expected_project_balance $
            accountBalance project_account'
