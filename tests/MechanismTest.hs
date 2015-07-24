{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module MechanismTest (mechanismSpecs) where

import Prelude
import TestImport hiding (get)
import Model.Currency
    (Milray (..), millMilray, dropRightZeros, pprintThousands)
import Model.Project (projectComputeShareValue, fetchProjectSharesDB)

import Control.Applicative ((<$>))
import Database.Esqueleto hiding (delete)
import qualified Database.Esqueleto as E
import Data.Int (Int64)
import Data.List (delete)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Test.QuickCheck
import Text.Printf (formatRealFloat, FieldFormat(..))

mechanismSpecs :: Spec
mechanismSpecs = ydescribe "mechanism" $ do
    testFormula
    testPledges
    testPPrinters
    testUnderfunded

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
              (millMilray $ fromIntegral n)

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
    ( project_name
     ,project_account
     ,user_account
     ,old_shares
     ,new_project_shares
     ,expected_share_value ) <- testDB $ do
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

data Symbol = SDollar | SCent

pprintSymbol :: Symbol -> String -> String
pprintSymbol SDollar str = "$" <> str
pprintSymbol SCent   str = str <> "¢"

-- | Test whether the results of applying a function to a positive value
-- and a negative value match the expectations.
errorUnlessExpectedPosAndNeg :: Text -> Symbol -> String -> (Milray -> String)
                             -> (Int64 -> Milray) -> Int64 -> Example ()
errorUnlessExpectedPosAndNeg msg symbol expected ppr con value = do
    errorUnlessExpected msg
        (pprintSymbol symbol expected)
        (ppr $ con value)
    errorUnlessExpected (msg <> ", negative amount")
        (pprintSymbol symbol ("-" <> expected))
        (ppr $ con $ negate value)

-- Using something like 'maxBound :: Int64' would be expensive because
-- of the way bounds are used in generators, and would lead to
-- rounding errors when divided.
bound :: Int64
bound = 10000000

-- | Split a string into the integral and fractional parts.
intAndFrac :: String -> (String, String)
intAndFrac = fmap (dropWhile (== '.')) . break (== '.')

-- | Drop zeros on the right and the dot if it's the last character.
dropRightZerosCent :: String -> String
dropRightZerosCent s = i <> (if null f' then "" else '.':f')
  where
    f'    = dropRightZeros f
    (i,f) = intAndFrac s

-- | Always show at least two decimal digits.
dropRightZerosDollar :: String -> String
dropRightZerosDollar s = i <> "." <> f''
    where
      f''   = if len < 2
              then f' <> replicate (2 - len) '0'
              else f'
      len   = length f'
      f'    = dropRightZeros f
      (i,f) = intAndFrac s

-- | Return the sign (if exists) and absolute value.
signAndAbs :: String -> (String, String)
signAndAbs ('-':xs) = ("-",xs)
signAndAbs xs       = ([] ,xs)

testCurrency :: Symbol -> Gen Int64 -> Int -> Int64 -> Property
testCurrency symbol gen len d =
    forAll gen $ \n ->
    let res = fromIntegral n / fromIntegral d :: Double
        (i,f) = intAndFrac $
                    formatRealFloat res
                       -- Do not use scientific notation for values
                       -- like 0.06.
                       (FieldFormat
                            Nothing (Just len) Nothing Nothing False "" 'f') ""
        (sign, absi) = signAndAbs i
        i' = sign <>
             (case symbol of
                  SCent   -> id
                  SDollar -> pprintThousands) absi
        pres = (case symbol of
                    SCent   -> dropRightZerosCent
                    SDollar -> dropRightZerosDollar) $ i' <> "." <> f
    in (show $ Milray n) ===
       pprintSymbol symbol pres

genStep :: Int64 -> Int64 -> Int64 -> Gen Int64
genStep start next end = elements $ sx <> xs
  where
    xs = [start, next .. end]
    sx = negate <$> reverse xs

testPPrinters :: Spec
testPPrinters = ydescribe "pretty-printers" $ do
    yit "Milray" $ do
        errorUnlessExpected "0 is printed correctly"
            "$0.00" $ show $ Milray 0
        errorUnlessExpectedPosAndNeg "milrays: leading zero is not removed"
            SCent "0.01" show Milray 1
        errorUnlessExpectedPosAndNeg "milrays: trailing zero is removed"
            SCent "0.1" show Milray 10

        errorUnlessExpectedPosAndNeg "cents: trailing zeros are removed"
            SCent "1" show Milray 100
        errorUnlessExpectedPosAndNeg
            ("cents: trailing zero is removed without " <>
             "affecting the second digit")
            SCent "1.2" show Milray 120
        errorUnlessExpectedPosAndNeg "cents: leading zero is not removed"
            SCent "1.03" show Milray 103
        errorUnlessExpectedPosAndNeg "cents: all digits are printed"
            SCent "1.23" show Milray 123
        errorUnlessExpectedPosAndNeg
            ("cents: trailing zeros are removed without " <>
             "affecting the second zero digit")
            SCent "10" show Milray 1000
        errorUnlessExpectedPosAndNeg
            ("cents: trailing zeros are removed without " <>
             "affecting the second non-zero digit")
            SCent "12" show Milray 1200
        errorUnlessExpectedPosAndNeg
            ("cents: trailing zero is removed without " <>
             "affecting the third digit")
            SCent "12.3" show Milray 1230
        errorUnlessExpectedPosAndNeg "cents: nothing is removed"
            SCent "12.34" show Milray 1234
        errorUnlessExpectedPosAndNeg
            "cents: leading zero is not removed (four digits)"
            SCent "12.04" show Milray 1204
        errorUnlessExpectedPosAndNeg
            "cents: first zero is not removed (four digits)"
            SCent "10.3" show Milray 1030
        errorUnlessExpectedPosAndNeg
            "cents: zeros in the middle are not removed"
            SCent "10.04" show Milray 1004

        errorUnlessExpectedPosAndNeg "dollars: trailing zeros are removed"
            SDollar "1.00" show Milray 10000
        errorUnlessExpectedPosAndNeg
            ("dollars: trailing zeros are removed without " <>
             "affecting the third zero digit")
            SDollar "1.20" show Milray 12000
        errorUnlessExpectedPosAndNeg
            ("dollars: trailing zeros are removed without " <>
             "affecting the third non-zero digit")
            SDollar "1.23" show Milray 12300
        errorUnlessExpectedPosAndNeg
            ("dollars: trailing zeros are removed without " <>
             "affecting the fourth digit")
            SDollar "1.234" show Milray 12340
        errorUnlessExpectedPosAndNeg "dollars: nothing is removed"
            SDollar "1.2345" show Milray 12345

        errorUnlessExpectedPosAndNeg "hundreds are not delimited"
            SDollar "123.45" show Milray 1234500
        errorUnlessExpectedPosAndNeg "thousands are delimited: 1"
            SDollar "1,234.56" show Milray 12345600
        errorUnlessExpectedPosAndNeg "thousands are delimited: 10"
            SDollar "12,345.6709" show Milray 123456709
        errorUnlessExpectedPosAndNeg "thousands are delimited: 100"
            SDollar "123,456.789" show Milray 1234567890

        testProperty "dollars are printed correctly" $
            testCurrency SDollar (genStep 10000 10001 bound) 4 10000
        testProperty "cents are printed correctly" $
           testCurrency SCent (genStep 1 2 9999) 2 100
        -- Whole cents are rarely generated by the above test, so it
        -- makes sense to explicitly check them.
        testProperty "whole cents are printed correctly" $
            testCurrency SCent (genStep 100 200 9900) 2 100
        -- Ditto.
        testProperty "whole dollars are printed correctly" $
            testCurrency SDollar (genStep 10000 20000 bound) 4 10000

fundedShares :: (Functor m, MonadIO m)
             => UserId -> ProjectId -> SqlPersistT m [Int64]
fundedShares user_id project_id =
    fmap (fmap unValue) $
    select $ from $ \p -> do
        where_ $ p ^. PledgeUser    ==. val user_id
             &&. p ^. PledgeProject ==. val project_id
        return $ p ^. PledgeFundedShares

-- | Delete all pledges to a project and set the share value to zero.
dropPledgesAndShareValue :: (Functor m, MonadIO m)
                         => ProjectId -> SqlPersistT m ()
dropPledgesAndShareValue project_id = do
    E.delete $ from $ \p ->
        where_ $ p ^. PledgeProject ==. val project_id
    E.update $ \p -> do
        set p [ProjectShareValue E.=. val (Milray 0)]
        where_ $ p ^. ProjectId ==. val project_id

errorUnlessFundedShares :: Text -> NamedUser -> ProjectId -> Int64
                        -> Example ()
errorUnlessFundedShares msg user project_id value = do
    user_id <- userId user
    testDB (fundedShares user_id project_id) >>=
        errorUnlessExpected msg [value]

setBalanceAndPledge :: NamedUser -> Milray -> ProjectId -> Int64 -> Example ()
setBalanceAndPledge user balance project_id pledge_value = do
    user_id <- userId user
    testDB $ setBalance user_id balance

    loginAs user
    pledge project_id pledge_value

    errorUnlessFundedShares
        (shpack user <> "'s shares")
        user project_id pledge_value

-- | Test whether user's pledge gets decreased when they can no longer
-- support the project because another patron joins.
testUnderfunded :: Spec
testUnderfunded = ydescribe "underfunded" $ do
    ydescribe "after new pledge" underfundedAfterPledge
    ydescribe "after payout" underfundedAfterPayout

underfundedAfterPledge :: Spec
underfundedAfterPledge = do
    yit "one underfunded patron, two patrons" $ do
        snowdrift_id <- snowdriftId
        testDB $ dropPledgesAndShareValue snowdrift_id

        -- Set Mary's balance to $1.00 and pledge $1.00.
        setBalanceAndPledge Mary (Milray 10000) snowdrift_id 1000
        -- Set Bob's balance to $20.00 and pledge $1.00.
        setBalanceAndPledge Bob (Milray 200000) snowdrift_id 1000

        errorUnlessFundedShares
            "Mary's shares after correction"
            Mary snowdrift_id 500

    yit "one underfunded patron, three patrons" $ do
        snowdrift_id <- snowdriftId
        testDB $ dropPledgesAndShareValue snowdrift_id

        -- Set Mary's balance to $2.00 and pledge $1.00.
        setBalanceAndPledge Mary (Milray 20000) snowdrift_id 1000
        -- Set Bob's balance to $12.00 and pledge $2.00.
        setBalanceAndPledge Bob (Milray 120000) snowdrift_id 2000
        -- Set Sue's balance to $27.00 and pledge $3.00.
        setBalanceAndPledge Sue (Milray 270000) snowdrift_id 3000

        errorUnlessFundedShares
            "Mary's shares after correction"
            Mary snowdrift_id 666

        errorUnlessFundedShares
            "Bob's shares after correction"
            Bob snowdrift_id 2000

    yit "one underfunded patron, four patrons" $ do
        snowdrift_id <- snowdriftId
        testDB $ dropPledgesAndShareValue snowdrift_id

        -- Set Mary's balance to $2.00 and pledge $1.00.
        setBalanceAndPledge Mary (Milray 20000) snowdrift_id 1000
        -- Set Bob's balance to $12.00 and pledge $2.00.
        setBalanceAndPledge Bob (Milray 120000) snowdrift_id 2000
        -- Set Sue's balance to $27.00 and pledge $3.00.
        setBalanceAndPledge Sue (Milray 270000) snowdrift_id 3000
        -- Set Joe's balance to $54.00 and pledge $4.00.
        setBalanceAndPledge Joe (Milray 540000) snowdrift_id 4000

        errorUnlessFundedShares
            "Mary's shares after correction"
            Mary snowdrift_id 500

        errorUnlessFundedShares
            "Bob's shares after correction"
            Bob snowdrift_id 2000

        errorUnlessFundedShares
            "Sue's shares after correction"
            Sue snowdrift_id 3000

underfundedAfterPayout :: Spec
underfundedAfterPayout = do
    yit "one patron, one project" $ do
        sid <- snowdriftId
        now <- liftIO getCurrentTime
        testDB $ dropPledgesAndShareValue sid
        -- Set balance to $1.00 and pledge 60¢
        setBalanceAndPledge Mary (Milray 10000) sid 600
        errorUnlessFundedShares
            "Mary's shares before payout"
            Mary sid 600
        -- Do the payment
        testDB $ insert_ $ Payday now
        processPayments "paid to Snowdrift" $ do
            -- Check Mary's funded_shares
            errorUnlessFundedShares
                "Mary's shares after payout"
                Mary sid 400
    -- yit "pay one project, defund another" $ return ()
