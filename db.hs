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

import Control.Exception.Base (bracket)
import Data.Maybe (catMaybes)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Prelude hiding (FilePath)
import System.Environment (getProgName)
import System.IO (openFile, IOMode(..))
import Turtle
import qualified Control.Foldl as F
import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as P
import qualified System.IO as H

main = sh $ do
    -- usageText
    (root, pghost, pgdata) <- initEnv
    initCluster root pghost  pgdata

initEnv :: Shell (FilePath, FilePath, FilePath)
initEnv = do
    root <- getProjectRoot
    let pghost = root </> ".postgres-work" </> "sockets"
        pgdata = root </> ".postgres-work" </> "data"
    export "PGHOST" (toText_ pghost)
    export "PGDATA" (toText_ pgdata)
    return (root, pghost, pgdata)

pgCtl args stdin' = do
    pgPath <- fromText <$> inshell "pg_config --bindir" ""
    procs (toText_ (pgPath </> "pg_ctl")) args stdin'

psql args = procs "psql" (["-v", "ON_ERROR_STOP="] <> args)

getProjectRoot =
    realpath =<< (directory . P.decodeString <$> liftIO getProgName)

usageText :: Shell ()
usageText = mapM_ stderr
    [ "db: a wrapper to set up environment variables and run various commands"
    , "for your local postgres database for Snowdrift.coop development."
    , ""
    , "USAGE:"
    , ""
    , "    db ACTION [ARGS]"
    , ""
    , "Where ACTION may be one of:"
    , ""
    , "    init            initialize the database cluster and databases"
    , "    clean           "
    , "    reset           "
    , "    export          create a data dump with pg_dump"
    , "    shell           just launch a subshell"
    , "    pg_ctl          run pg_ctl"
    , "    psql            connect to the databse with psql"
    , "    exec COMMAND    run COMMAND"
    ]

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
    err "Creating databases..."
    psql ["postgres"] $ select
        [ "create database snowdrift_development;"
        , "create database snowdrift_test_template;"
        , "update pg_database set datistemplate=true where datname='snowdrift_test_template';"
        ]
    err "Loading devDB..."
    psql ["snowdrift_development"] $ input "devDB.sql"
    err "Loading testDB..."
    psql ["snowdrift_test_template"] $ input "testDB.sql"
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

-- ##
-- ## Helper functions/additions to underlying libs
-- ##

-- | Force convert a path.
toText_ :: FilePath -> Text
toText_ p = case toText p of
    Right p' -> p'
    _ -> error "Could not convert FilePath to Text"

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

-- 		echo >&2 "Creating databases..."
-- 		psql postgres <<-EOF
-- 			CREATE DATABASE ${PGDATABASE};
-- 			CREATE DATABASE ${PGDATABASE}_test_template;
-- 			UPDATE pg_database SET datistemplate=true WHERE datname='${PGDATABASE}_test_template';
-- 			EOF
-- 		echo >&2 "Loading devDB..."
-- 		grep -v "OWNER TO\|GRANT\|REVOKE" "$project_root"/devDB.sql | psql
-- 		echo >&2 "Loading testDB..."
-- 		grep -v "OWNER TO\|GRANT\|REVOKE" "$project_root"/testDB.sql | psql "${PGDATABASE}_test_template" > "$project_root"/init-test-db.log
-- 		echo >&2 "Success."
-- 		;;
-- 	"reset")
-- 		"$0" clean
-- 		"$0" init
-- 		;;
-- 	"clean")
-- 		"$0" stop
-- 		rm -r "$project_root"/postgres/
-- 		;;
-- 	"start")
-- 		exec pg_ctl start	;;
-- 	"stop")
-- 		exec pg_ctl stop	;;
-- 	"shell")
-- 		exec "$SHELL" "$@"	;;
-- 	"pg_ctl")
-- 		exec "pg_ctl" "$@"	;;
-- 	"psql")
-- 		exec "psql" "$@"	;;
-- 	"exec")
-- 		exec "$@"			;;
-- 	*)
-- 		echo >&2 "Error: unknown command $command"
-- 		exit 2
-- 		;;
-- esac
