{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Core definitions for the library.
--
-- This is an internal module. Kindly direct your attention to
-- "Crowdmatch", which is the user API.
--
-- Note in particular that this module defines a Patron type that is
-- /not/ the type exported at the top level!
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

-- | This class is used to associate some \'u\' from your model with
-- Crowdmatch's internal Patron type.
class ToMechPatron u where
    toMechPatron :: u -> Int
    fromMechPatron :: Int -> u

external :: ToMechPatron u => Iso' PPtr u
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
