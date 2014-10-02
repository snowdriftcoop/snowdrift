{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

import Prelude hiding (init, length)
import Control.Exception (bracket)
import Control.Monad hiding (forM_)
import Data.List.NonEmpty hiding (init, words, unwords)
import Data.Foldable (forM_)
import Data.Monoid ((<>))
import System.Console.CmdArgs hiding (args)
import System.Directory
import System.Environment
import System.IO
import System.Process

-- Command line interface.

main :: IO ()
main = do
  exists <- doesFileExist "Snowdrift.cabal"
  unless exists $ error "please run from the project's root directory"
  Sdm {..} <- getProgName >>= cmdArgs . sdm
  handle action db

data Sdm = Sdm
  { db     :: String
  , action :: String
  } deriving (Typeable, Data, Show)

sdm :: String -> Sdm
sdm pname = Sdm
  { db = "all"                  -- operate on the dev and test databases
      &= help "Database to operate on"
      &= explicit &= name "db"
      &= typ "DATABASE"
  , action = def &= argPos 0 &= typ "ACTION"
  } &= summary "Snowdrift database manager 0.1" &= program pname
    &= details [ "Actions: init, clean, reset"
               , "Databases: dev, test, all (default)" ]

handle :: String -> String -> IO ()
handle action db
  -- Force evaluation to check that the 'db' argument is valid.
  | action == "init"  = init  $! parse db
  | action == "clean" = clean $! parse db
  | action == "reset" = reset $! parse db
  | otherwise         = error $ "invalid action; must be one of: "
                     <> "init, clean, reset"

parse :: String -> NonEmpty DB
parse s
  | s == "dev"  = fromList [dev]
  | s == "test" = fromList [test]
  | s == "all"  = fromList [dev, test]
  | otherwise   = error $ "invalid argument to 'db': "
               <> "must be one of: all, dev, test"

type Alias = String
data DB = Dev Alias DBInfo | Test Alias DBInfo

dbAlias :: DB -> Alias
dbAlias (Dev a _)  = a
dbAlias (Test a _) = a

dbInfo :: DB -> DBInfo
dbInfo (Dev _ i)  = i
dbInfo (Test _ i) = i

data DBInfo = DBInfo { dbFile  :: String
                     , dbUser  :: String
                     , dbName  :: String
                     } deriving (Eq, Show)

dev, test :: DB
dev = Dev "dev" $ DBInfo { dbFile = "devDB.sql"
                         , dbUser = "snowdrift_development"
                         , dbName = "snowdrift_development" }

test = Test "test" $ DBInfo { dbFile = "testDB.sql"
                            , dbUser = "snowdrift_test"
                            , dbName = "snowdrift_test_template" }


-- Shell commands.

infixl 2 -|-
(-|-) :: CreateProcess -> CreateProcess -> IO String
x -|- y = do
  (_, Just ox, _, _) <- createProcess x { std_out = CreatePipe }
  (_, Just oy, _, _) <- createProcess y { std_in  = UseHandle ox
                                        , std_out = CreatePipe }
  hGetContents oy

run :: String -> [String] -> IO ()
run cmd args = void $ rawSystem cmd args

postgres :: [String] -> CreateProcess
postgres args = proc "sudo" $ ["-u", "postgres"] ++ args

psql :: String -> IO ()
psql arg = putStr =<< proc "echo" [arg] -|- postgres ["psql"]

cat :: String -> CreateProcess
cat file = proc "cat" [file]


-- Database interaction.

dropDB, createDB :: DBInfo -> IO ()
dropDB DBInfo {..}   = psql $ "DROP DATABASE " <> dbName <> ";"
createDB DBInfo {..} = psql $ "CREATE DATABASE " <> dbName <> ";"

importDB :: DBInfo -> IO ()
importDB DBInfo {..} = putStr =<< cat dbFile -|- postgres ["psql", dbName]

createUser :: DBInfo -> [String] -> IO ()
createUser DBInfo {..} opts = psql $ "CREATE USER " <> dbUser <> " "
                           <> unwords opts <> ";"

dropRole :: DBInfo -> IO ()
dropRole DBInfo {..} = psql $ "DROP ROLE " <> dbUser <> ";"

alterUser :: DBInfo -> String -> IO ()
alterUser DBInfo {..} password = psql $ "ALTER USER " <> dbUser
                              <> " WITH ENCRYPTED PASSWORD '" <> password <> "';"

template :: Bool -> DBInfo -> IO ()
template b DBInfo {..}  = psql $ "UPDATE pg_database SET datistemplate=" <> show b
                       <> " WHERE datname='" <> dbName <> "';"

setTemplate, unsetTemplate :: DBInfo -> IO ()
setTemplate   = template True
unsetTemplate = template False

doesDBExist :: DBInfo -> IO Bool
doesDBExist DBInfo {..}  = do
  dbs <- postgres ["psql", "-lqt"] -|- proc "cut" ["-d|", "-f1"]
  return . elem dbName $ words dbs


-- Actions.

config :: String
config = "config/postgresql.yml"

getPassword :: IO String
getPassword =
  bracket (hGetEcho stdout) (hSetEcho stdout) . const $ do
    putStr "Password: "
    hSetEcho stdout False
    hFlush stdout
    p <- getLine
    putStr "\n"
    return p

init, clean, reset :: NonEmpty DB -> IO ()
init ((Test {}) :| []) = error "cannot initialize only test; try to init dev or all"
init dbs = do
  exists <- doesFileExist config
  when exists $ error "already initialized; doing nothing"

  -- Get a database password from a user.
  putStrLn "Please specify a database password you would like to use."
  password <- getPassword
  when (null password) $ error "no password provided"

  -- Setup the databases.
  run "cp" ["config/postgresql.template", config]
  forM_ dbs $ \db -> init' db password

  run "sed" [ "-i", "s/REPLACE WITH YOUR PASSPHRASE/" <> password <> "/"
            , config ]
  run "sudo" ["chmod", "400", config]
    where
      init' :: DB -> String -> IO ()
      init' (Dev _ info) password = do
        createUser info ["NOSUPERUSER", "NOCREATEDB", "NOCREATEROLE"]
        createDB info
        alterUser info password
        psql $ "GRANT ALL PRIVILEGES ON DATABASE " <> dbName info
            <> " TO " <> dbUser info <> ";"
        importDB info

      init' (Test _ info) password = do
        createUser info ["NOSUPERUSER", "CREATEDB", "NOCREATEROLE"]
        createDB info
        alterUser info password
        setTemplate info
        importDB info

ifExists :: DB -> IO () -> IO ()
ifExists db as = do
  exists <- doesDBExist $ dbInfo db
  if exists
    then as
    else do
      -- Don't error out or it won't check all 'dbs'.
      pname <- getProgName
      putStrLn $ pname <> ": " <> dbAlias db <> " does not exist; doing nothing"

clean dbs
  | length dbs == 1 =
      error $ "cannot clean a single database; try to reset it or to clean all"
  | otherwise = do
      run "rm" ["-f", config]
      forM_ dbs clean'
  where
    dropDBAndRole info = dropDB info >> dropRole info
    clean' db@(Dev _ info)  = ifExists db $ dropDBAndRole info
    clean' db@(Test _ info) = ifExists db $ do
      unsetTemplate info
      dropDBAndRole info

reset dbs = forM_ dbs $ \db -> do
  let info = dbInfo db
  ifExists db $ do
    dropDB info
    createDB info
    importDB info
