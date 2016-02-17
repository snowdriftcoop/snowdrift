#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import System.Environment (getProgName)
import qualified Filesystem.Path.CurrentOS as P

-- # db - a one-off script to manage a local development postgres cluster, without
-- # sudo. Written for snowdrift.coop, but applicable to any project. Mostly just
-- # sets some environment variables appropriately and wraps pg_ctl.
-- #
-- # Copyright 2015 Michael F. Lamb
-- # License: AGPLv3+

-- # Notes:
-- #
-- # Listening only on a socket won't work on Windows, which doesn't have unix
-- # sockets.
-- #

toText_ p = case toText p of
    Right p' -> p'
    _ -> error "Could not convert FilePath to Text"

main = sh $ do
    -- usageText
    (root, pghost, pgdata) <- initEnv
    initCluster root pghost  pgdata

-- initEnv :: Shell (FilePath, FilePath, FilePath)
initEnv = do
    root <- getProjectRoot
    let pghost = root </> ".postgres-work" </> "sockets"
        pgdata = root </> ".postgres-work" </> "data"
    export "PGHOST" (toText_ pghost)
    export "PGDATA" (toText_ pgdata)
    return (root, pghost, pgdata)

pgCtl args stdinn = do
    pgPath <- fromText <$> inshell "pg_config --bindir" ""
    procs (toText_ (pgPath </> "pg_ctl")) args stdinn

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
initCluster root pghost pgdata = do
    let pghostT = toText_ pghost
        pgdataT = toText_ pgdata
    err "Creating directories.."
    mktree pghost
    mktree pgdata
    err "Initializing cluster..."
    pgCtl ["initdb", "-o", "--nosync --auth=peer", "-D", pgdataT] ""
    err "Updating cluster configuration file..."
    setPgConfigOpts
        (pgdata </> "postgresql.conf")
        [ -- set the unix socket directory because pg_ctl start doesn't
          -- pay attention to PGHOST (wat.)
          ("unix_socket_directory", pghostT)
        , ("unix_socket_directories", pghostT)
        , ("archive_mode", "off")
        , ("fsync", "off")
        , ("wal_level", "minimal")
          -- don't bother listening on a port, just a socket.
        , ("listen_addresses", "")
        ]
  where
    setPgConfigOpts f opts = inplace (choice (map (uncurry optSettingPattern) opts)) f

optSettingPattern opt value = do
    _ <- do
        begins (star (oneOf " #"))
        text opt
        oneOf " ="
        suffix (star dot)
    return (opt <> " = " <> value)

-- command="$1"
-- shift
-- case "$command" in
-- 	"init")
-- 		logfile="$project_root"/init-dev-db.log
-- 		exec > $logfile
-- 		echo >&2 "Logging to $logfile..."
-- 		echo >&2 "Creating directories..."
-- 		mkdir -p "$PGHOST" "$PGDATA"
-- 		echo >&2 "Initializing cluster..."
-- 		pg_ctl initdb -o "--nosync --auth=peer" -D "$PGDATA"
-- 		echo >&2 "Updating cluster configuration file..."
-- 		sed -i -e "
-- 			# set the unix socket directory because pg_ctl start doesn't pay attention
-- 			# to PGHOST (wat.)
-- 			s|^[ #]*\(unix_socket_directory\)[ =].*$|\1 = '$PGHOST'|
-- 			s|^ [ #]*\(unix_socket_directory\)   [ =].*$|\1 = '$PGHOST'|
-- 			s|^ [ #]*\(unix_socket_directories\) [ =].*$|\1 = '$PGHOST'|
-- 			s|^ [ #]*\(archive_mode\)            [ =].*$|\1 = off|
-- 			s|^ [ #]*\(fsync\)                   [ =].*$|\1 = off|
-- 			s|^ [ #]*\(full_page_writes\)        [ =].*$|\1 = off|
-- 			s|^ [ #]*\(wal_level\)               [ =].*$|\1 = minimal|
-- 			# don't bother listening on a port, just a socket.
-- 			s|^[ #]*\(listen_addresses\)[ =].*$|\1 = ''|
-- 			# delete all the other commented-out lines stuck in that file by default.
-- 			/^[\t ]*#/d
-- 			/^[\t ]*$/d
-- 			" "$PGDATA/postgresql.conf"
-- 		echo >&2 "Starting database server..."
-- 		pg_ctl start -w
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
