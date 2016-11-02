{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Core definitions for the library
module Crowdmatch.Model (module Crowdmatch.Model) where

import Control.Lens
import Data.Monoid ((<>))
import Control.Monad.IO.Class
import Data.Time
import Database.Persist.TH
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration

import Crowdmatch.ModelDataTypes as Crowdmatch.Model

share [mkPersist sqlSettings
      , mkMigrate "migrateCrowdmatch"
      ] [persistLowerCase|
Patron sql="crowdmatch__patron"
    usr PPtr
    created UTCTime
    paymentToken PaymentToken Maybe
    donationPayable DonationUnits
    pledgeSince UTCTime Maybe
    --
    UniquePatron usr

PledgeHistory sql="crowdmatch__pledge_history"
    patron PatronId
    time UTCTime
    action PledgeAction
    --
    deriving (Show)

DonationHistory sql="crowdmatch__donation_history"
    patron PatronId
    time DonationTime
    amount DonationUnits
    fee Cents
    --
    deriving (Show)

CrowdmatchHistory sql="crowdmatch__crowdmatch_history"
    patron PatronId
    date CrowdmatchDay
    amount DonationUnits
    --
    deriving (Show)
|]

class ToMechPatron u where
    toMechPatron :: u -> Int
    fromMechPatron :: Int -> u

external :: ToMechPatron u => Iso' PPtr u
external = iso toExternal fromExternal
  where
    toExternal (PPtr i) = fromMechPatron i
    fromExternal = PPtr . toMechPatron

-- | Manual migrations to run
crowdmatchManualMigrations :: MonadIO io => Connection -> io ()
crowdmatchManualMigrations con = liftIO $
    withTransaction con $ do
        _ <- runMigration (MigrationContext MigrationInitialization True con)
        mapM_ (runMigration . inContext) migrations
  where
    inContext (name,script) =
        MigrationContext (MigrationScript name script) True con
    migrations = let (..=) = (,) in
        [ "payment-capability"
            ..= ("alter table crowdmatch__patron"
                <> " add constraint pledge_capability"
                <> " check (pledge_since is null"
                    <> " or payment_token is not null)")
        ]
