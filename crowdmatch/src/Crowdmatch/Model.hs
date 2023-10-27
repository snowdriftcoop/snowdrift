{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,10,0)
{-# LANGUAGE DerivingStrategies #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

-- | Core definitions for the library.
--
-- This is an internal module. Kindly direct your attention to
-- "Crowdmatch", which is the user API.
--
-- Note in particular that this module defines a Patron type that is
-- /not/ the type exported at the top level!
module Crowdmatch.Model (module Crowdmatch.Model) where

import Control.Lens (Iso', iso)
import Data.Monoid ((<>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime)
import Database.Persist.TH
        (share, mkPersist, mkMigrate, persistLowerCase, sqlSettings)
import Database.PostgreSQL.Simple (Connection, withTransaction)
import Database.PostgreSQL.Simple.Migration
        (runMigration, MigrationContext(..), MigrationCommand(..))

import Crowdmatch.ModelDataTypes as Crowdmatch.Model

 -- PaymentToken is whatever we get back from Stripe when the user adds a credit card
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
    deriving Show Eq

PledgeHistory sql="crowdmatch__pledge_history"
    patron PatronId
    time UTCTime
    action StorageAction
    --
    deriving (Show)

PaymentTokenHistory sql="crowdmatch__payment_token_history"
    patron PatronId
    time HistoryTime
    action StorageAction

DonationHistory sql="crowdmatch__donation_history"
    patron PatronId
    time HistoryTime
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

-- | This class is used to associate some \'u\' from your model with
-- Crowdmatch's internal Patron type.
class ToCrowdmatchPatron u where
    toMechPatron :: u -> Int
    fromMechPatron :: Int -> u

external :: ToCrowdmatchPatron u => Iso' PPtr u
external = iso toExternal fromExternal
  where
    toExternal (PPtr i) = fromMechPatron i
    fromExternal = PPtr . toMechPatron

-- | Manual crowdmatch migrations to run. FIXME: right now this has to be
-- run /after/ automatic migrations, but eventually most manual migrations
-- will need to be run /before/ automatic migrations. That's really weird.
-- It also sucks that this method cannot reuse any persistent machinery.
--
-- I can't create haddocks for 'migrateCrowdmatch', so I will write about
-- it here. It is a Persistent-generated automatic migration. Make sure to
-- sequence it with your own model's migrations.
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
