module Migrations
    ( doManualMigration
    , saveUnsafeMigrations
    , migrateTriggers
    ) where

import Import

import Control.Monad.Trans.Reader
import System.Directory
import System.IO (stderr)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

runSql :: MonadIO m => Text -> ReaderT SqlBackend m ()
runSql = flip rawExecute []

doManualMigration :: (MonadIO m, MonadLogger m, Functor m)
                  => SqlPersistT m ()
doManualMigration = do
    deprecatedApplyManualMigrations
    saveUnsafeMigrations

deprecatedApplyManualMigrations
    :: (MonadLogger m, MonadIO m)
    => ReaderT SqlBackend m ()
deprecatedApplyManualMigrations = do
    $(logInfo) "creating version table"

    runSql "CREATE TABLE IF NOT EXISTS \"database_version\" (\"id\" SERIAL PRIMARY KEY UNIQUE, \"last_migration\" INT8 NOT NULL);"

    last_migration <- select $ from return

    migration_number <- case last_migration of
        [] -> insert (DatabaseVersion 0) >> return 0
        [Entity _ (DatabaseVersion migration)] -> return migration
        _ -> error "multiple entries in DB version table"

    unfiltered_migration_files <- liftIO $ getDirectoryContents "migrations"

    let migration_files :: [(Int, String)]
        migration_files = L.sort
            $ L.filter ((> migration_number) . fst)
            $ mapMaybe (\s -> fmap (,s) $ readMaybe =<< L.stripPrefix "migrate" s)
            unfiltered_migration_files

    forM_ (L.map (("migrations/" <>) . snd) migration_files) $ \file -> do
        $(logWarn) $ "running " <> T.pack file <> "..."
        migration <- liftIO $ T.readFile file
        runSql migration

    let new_last_migration = L.maximum $ migration_number : L.map fst migration_files

    update $ flip set [ DatabaseVersionLastMigration =. val new_last_migration ]

    -- This is where new safe migrations used to be recorded.

-- | Automatically save unsafe migrations, if they exist.
--
-- We'll still blow up with runMigration, but we'll be one step closer to
-- storing the necessary sql statements to commit and share.
saveUnsafeMigrations :: (MonadIO m, Functor m) => ReaderT SqlBackend m ()
saveUnsafeMigrations = do
    unsafe <- L.filter fst <$> parseMigration' migrateAll
    unless (L.null $ L.map snd unsafe) $ do
        liftIO $ T.writeFile filename $ T.unlines $ L.map ((`T.snoc` ';') . snd) unsafe
        liftIO $ mapM_ (T.hPutStrLn stderr) unsafeMigMessages
  where
    filename = "migrations/migrate.unsafe"
    unsafeMigMessages =
      [ ""
      , "*** UNSAFE MIGRATIONS EXIST"
      , ""
      , "The application will now exit. But first, unsafe migrations have been"
      , "stored in '" <> filename <> "'. Please review them, rename them"
      , "appropriately, and commit them with your change."
      , ""
      , "More information follows."
      , ""
      ]


migrateTriggers :: MonadIO m => ReaderT SqlBackend m ()
migrateTriggers = do
    runSql $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_role_event_trigger() RETURNS trigger AS $role_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'DELETE') THEN"
        , "            INSERT INTO role_event (ts, \"user\", role, project, added) SELECT now(), OLD.\"user\", OLD.role, OLD.project, 'f';"
        , "            RETURN OLD;"
        , "        ELSIF (TG_OP = 'INSERT') THEN"
        , "            INSERT INTO role_event (ts, \"user\", role, project, added) SELECT now(), NEW.\"user\", NEW.role, NEW.project, 't';"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$role_event$ LANGUAGE plpgsql;"
        ]

    runSql "DROP TRIGGER IF EXISTS role_event ON project_user_role;"

    runSql $ T.unlines
        [ "CREATE TRIGGER role_event"
        , "AFTER INSERT OR DELETE ON project_user_role"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_role_event_trigger();"
        ]

    runSql $ T.unlines
        [ "CREATE OR REPLACE FUNCTION log_doc_event_trigger() RETURNS trigger AS $doc_event$"
        , "    BEGIN"
        , "        IF (TG_OP = 'INSERT' OR TG_OP = 'UPDATE') THEN"
        , "            INSERT INTO doc_event (ts, doc, blessed_version) SELECT now(), NEW.id, NEW.current_version;"
        , "            RETURN NEW;"
        , "        END IF;"
        , "        RETURN NULL;"
        , "    END;"
        , "$doc_event$ LANGUAGE plpgsql;"
        ]

    runSql "DROP TRIGGER IF EXISTS doc_event ON doc;"

    runSql $ T.unlines
        [ "CREATE TRIGGER doc_event"
        , "AFTER INSERT OR DELETE ON doc"
        , "    FOR EACH ROW EXECUTE PROCEDURE log_doc_event_trigger();"
        ]

    return ()
