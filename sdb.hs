#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}

-- | db - a one-off script to manage a local development postgres cluster,
-- without sudo. Written for snowdrift.coop, but applicable to any project.
-- Mostly just sets some environment variables appropriately and wraps
-- pg_ctl.
--
-- Copyright 2015 Michael F. Lamb
-- License: AGPLv3+
--
-- Converted to Haskell/turtle by Bryan Richter.
--
-- Notes:
--
-- Listening only on a socket won't work on Windows, which doesn't have
-- unix sockets.

import Prelude hiding (FilePath)

import Control.Exception.Base (bracket)
import Data.Maybe (catMaybes)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Environment (getProgName, getArgs)
import System.IO (openFile, IOMode(..))
import System.Process (callProcess)
import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P
import qualified System.IO as H

pgWorkDir = ".postgres-work"

usageText :: Shell ()
usageText = mapM_ err
    [ "sdb.hs: a wrapper to set up environment variables and run various commands"
    , "for your local postgres database for Snowdrift.coop development."
    , ""
    , "USAGE:"
    , ""
    , "    sdb.hs ACTION [ARGS]"
    , ""
    , "Where ACTION may be one of:"
    , ""
    , "    init         initialize the database cluster and databases"
    , "    start        (re)start the cluster"
    , "    stop         stop the cluster"
    , "    clean"
    , "    reset"
    , "    export       create data dumps with pg_dump"
    , "    pg_ctl       run pg_ctl"
    , "    psql         connect to the databse with psql"
    ]

main = sh $ do
    (root, pghost, pgdata) <- initEnv

    let init'  = initCluster root pghost pgdata
        clean' = rmRF (root </> pgWorkDir)
        stop'  = pgCtlQuietly ["stop"] empty

    args <- liftIO getArgs
    case args of
        ["init"]       -> init'
        ["start"]      -> pgCtl ["start"] empty
        ["stop"]       -> stop'
        ["clean"]      -> stop' >> clean'
        ["reset"]      -> stop' >> clean' >> init'
        ["export"]     -> exportDb
        ("pg_ctl":as') -> pgCtl (map T.pack as') empty
        ("psql":as')   -> liftIO $ callProcess "psql" as'
        ["--help"]     -> usageText
        _ -> do
            err ""
            err "** Unknown or missing options! **"
            err ""
            usageText
            exit (ExitFailure 1)

exportDb :: Shell ()
exportDb = do
    output "devDB.sql" $ pgDump "snowdrift_development"
    output "testDB.sql" $ pgDump "snowdrift_test_template"
  where
    pgDump db = inproc "pg_dump" ["--no-owner", "--no-privileges", "--create", db] empty

initCluster :: FilePath -> FilePath -> FilePath -> Shell ()
initCluster root pghost pgdata = redirected logfile $ do
    err ("Logging to " <> logfile')

    err "Creating directories.."
    mktree pghost
    mktree pgdata

    err "Initializing cluster..."
    pgCtl ["initdb", "-o", "--nosync --auth=peer", "-D", pgdata'] empty

    err "Updating cluster configuration file..."
    setPgConfigOpts (pgdata </> "postgresql.conf")
        [ -- set the unix socket directory because pg_ctl start doesn't
          -- pay attention to PGHOST (wat.)
          ("unix_socket_directory", pghost')
        , ("unix_socket_directories", pghost')
        , ("archive_mode", "off")
        , ("fsync", "off")
        , ("wal_level", "minimal")
          -- don't bother listening on a port, just a socket.
        , ("listen_addresses", "''")
        ]

    err "Starting database server..."
    pgCtl ["start", "-w"] empty

    err "Creating and populating databases..."
    psql ["postgres"] $ input "devDB.sql"
    psql ["postgres"] $ input "testDB.sql"
    psql ["postgres"] $ select
        [ "update pg_database set datistemplate=true where datname='snowdrift_test_template';"
        ]

    err "Writing old-skool config file..."
    Just user <- need "USER"
    output "config/postgresql.yml" $ select (dbConfigTemplate user pghost')

    err "Success."

  where

    pghost'  = "'" <> toText_ pghost <> "'"
    logfile  = root </> "init-dev-db.log"
    logfile' = toText_ logfile
    pgdata'  = toText_ pgdata
    setPgConfigOpts f opts =
        inplace_ (choice (patterns opts)) f
    patterns opts =
        map (fmap Just . uncurry optSettingPattern) opts
        <> [ commentOrEmpty >> pure Nothing ]
    commentOrEmpty :: Pattern Text
    commentOrEmpty =
        contains (begins spaces >> (T.singleton <$> (char '#' <|> newline)))
    optSettingPattern opt value = do
        -- match the line with the option
        _ <- contains $ do
            begins (star (oneOf " #"))
            text opt
            once (oneOf " =")
        -- replace it with 'opt = value'
        return (opt <> " = " <> value)

-- | Template for database config file.
dbConfigTemplate user pghost =
    [ "Default: &defaults"
    , format ("   user: "%s) user
    , "   password: \"\""
    , format ("   host: "%s) pghost
    , "   database: snowdrift_development"
    , "   poolsize: 10"
    , ""
    , "Development:"
    , "  <<: *defaults"
    , ""
    , "Testing:"
    , "  database: snowdrift_test"
    , "  <<: *defaults"
    , ""
    , "Staging:"
    , "  database: snowdrift_staging"
    , "  poolsize: 100"
    , "  <<: *defaults"
    , ""
    , "Production:"
    , "  <<: *defaults"
    ]

-- | Create and export some env variables
initEnv :: Shell (FilePath, FilePath, FilePath)
initEnv = do
    root <- getProjectRoot
    let pghost = root </> pgWorkDir </> "sockets"
        pgdata = root </> pgWorkDir </> "data"
    export "PGHOST" (toText_ pghost)
    export "PGDATA" (toText_ pgdata)
    return (root, pghost, pgdata)

-- | A fail-early version for general use
pgCtl args stdin' = do
    pgPath <- fromText <$> inshell "pg_config --bindir" ""
    procs (toText_ (pgPath </> "pg_ctl")) args stdin'

-- | A version that does not throw errors all up ins
pgCtlQuietly args stdin' = hush $ do
    pgPath <- fromText <$> inshell "pg_config --bindir" ""
    void $ proc (toText_ (pgPath </> "pg_ctl")) args stdin'

-- | A fail-early version of psql
psql args = procs "psql" (["-v", "ON_ERROR_STOP="] <> args)

getProjectRoot =
    realpath =<< (directory . P.decodeString <$> liftIO getProgName)

-- ##
-- ## Helper functions/additions to underlying libs
-- ##

-- | Seeing as I use this everywhere
toText_ :: FilePath -> Text
toText_ = format fp

-- | inplace with filtering.
inplace_ :: MonadIO io => Pattern (Maybe Text) -> FilePath -> io ()
inplace_ pattern file = liftIO (runManaged (do
    here              <- pwd
    (tmpfile, handle) <- mktemp here "turtle"
    outhandle handle (sed_ pattern (input file))
    liftIO (H.hClose handle)
    mv tmpfile file ))

-- | sed with filtering.
sed_ :: Pattern (Maybe Text) -> Shell Text -> Shell Text
sed_ pattern s = flatten $ do
    when (matchesEmpty pattern) (die message)
    let pattern' = fmap mconcat
            (many (pattern <|> fmap (Just . T.singleton) anyChar))
    txt    <- s
    txt':_ <- return (match pattern' txt)
    return txt'
  where
    message = "sed: the given pattern matches the empty string"
    matchesEmpty = not . null . flip match ""
    flatten s = do
        Just x <- s
        return x

-- | Run a shell but redirect stdout to a file
redirected :: FilePath -> Shell () -> Shell ()
redirected f s = liftIO $ bracket
    (do
        save <- hDuplicate H.stdout
        h <- openFile (P.encodeString f) WriteMode
        hDuplicateTo h H.stdout
        return save
    )
    (\h -> hDuplicateTo h H.stdout)
    (const (sh s))

-- | Run a shell and send stderr to nowhere
hush :: Shell () -> Shell ()
hush s = liftIO $ bracket
    (do
        save <- hDuplicate H.stderr
        h <- openFile "/dev/null" WriteMode
        hDuplicateTo h H.stderr
        return save
    )
    (\h -> hDuplicateTo h H.stderr)
    (const (sh s))

-- | rm -rf a directory. thx.
rmRF dir = do
    True <- testdir dir
    rmtree dir
