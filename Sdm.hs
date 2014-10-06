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
  run "sudo" ["-K"]  -- require a password the first time 'sudo' is run
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

dbInfo :: DB -> DBInfo
dbInfo (Dev _ i)  = i
dbInfo (Test _ i) = i

newtype DBFile = DBFile String
newtype DBUser = DBUser String
newtype DBName = DBName String
newtype DBTemp = DBTemp String

data DBInfo = DBInfo { dbFile :: DBFile
                     , dbUser :: DBUser
                     , dbName :: DBName
                     , dbTemp :: Maybe DBTemp }

class ToString a => DBType a
instance DBType DBName
instance DBType DBTemp

class ToString a where
  toString :: a -> String

instance ToString DBUser where
  toString (DBUser u) = u

instance ToString DBName where
  toString (DBName n) = n

instance ToString DBTemp where
  toString (DBTemp n) = n

dev, test :: DB
dev = Dev "dev" $
  DBInfo { dbFile = DBFile "devDB.sql"
         , dbUser = DBUser "snowdrift_development"
         , dbName = DBName "snowdrift_development"
         , dbTemp = Nothing }

test = Test "test" $
  DBInfo { dbFile = DBFile "testDB.sql"
         , dbUser = DBUser "snowdrift_test"
         , dbName = DBName "snowdrift_test"
         , dbTemp = Just $ DBTemp "snowdrift_test_template" }


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

dropDB, createDB :: DBType a => a -> IO ()
dropDB dbType   = psql $ "DROP DATABASE " <> toString dbType <> ";"
createDB dbType = psql $ "CREATE DATABASE " <> toString dbType <> ";"

importDB :: DBType a => DBFile -> a -> IO ()
importDB (DBFile f) dbType = putStr =<< cat f
                         -|- postgres ["psql", toString dbType]

createUser :: DBUser -> [String] -> IO ()
createUser (DBUser u) opts = psql $ "CREATE USER " <> u <> " "
                          <> unwords opts <> ";"

dropRole :: DBUser -> IO ()
dropRole (DBUser u) = psql $ "DROP ROLE " <> u <> ";"

alterUser :: DBUser -> String -> IO ()
alterUser (DBUser u) password = psql $ "ALTER USER " <> u
                             <> " WITH ENCRYPTED PASSWORD '" <> password <> "';"

template :: Bool -> DBTemp -> IO ()
template b (DBTemp n) = psql $ "UPDATE pg_database SET datistemplate="
                     <> show b <> " WHERE datname='" <> n <> "';"

setTemplate, unsetTemplate :: DBTemp -> IO ()
setTemplate   = template True
unsetTemplate = template False

doesDBExist :: DBType a => a -> IO Bool
doesDBExist dbType = do
  dbs <- postgres ["psql", "-lqt"] -|- proc "cut" ["-d|", "-f1"]
  return . elem (toString dbType) $ words dbs


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
      init' (Dev _ (DBInfo {..})) password = do
        createUser dbUser ["NOSUPERUSER", "NOCREATEDB", "NOCREATEROLE"]
        createDB dbName
        alterUser dbUser password
        psql $ "GRANT ALL PRIVILEGES ON DATABASE " <> toString dbName
            <> " TO " <> toString dbUser <> ";"
        importDB dbFile dbName

      init' (Test _ (DBInfo {..})) password =
        forM_ dbTemp $ \dbTemp' -> do
          createUser dbUser ["NOSUPERUSER", "CREATEDB", "NOCREATEROLE"]
          createDB dbTemp'
          alterUser dbUser password
          setTemplate dbTemp'
          importDB dbFile dbTemp'

ifExists :: DBType a => a -> IO () -> IO ()
ifExists dbType as = do
  exists <- doesDBExist dbType
  if exists
    then as
    else do
      -- Don't error out or it won't check all 'dbs'.
      pname <- getProgName
      putStrLn $ pname <> ": " <> (toString dbType)
              <> " does not exist; doing nothing"

clean dbs
  | length dbs == 1 =
      error $ "cannot clean a single database; try to reset it or to clean all"
  | otherwise = do
      run "rm" ["-f", config]
      forM_ dbs clean'
  where
    dropDBAndRole db role = dropDB db >> dropRole role
    clean' (Dev _ (DBInfo {..})) =
      ifExists dbName $ dropDBAndRole dbName dbUser
    clean' (Test _ (DBInfo {..})) = do
      forM_ dbTemp $ \dbTemp' ->
        ifExists dbTemp' $ do
          unsetTemplate dbTemp'
          dropDB dbTemp'
      ifExists dbName $ dropDBAndRole dbName dbUser

reset dbs = forM_ dbs $ \db -> do
  let info    = dbInfo db
      dbName' = dbName info
  ifExists dbName' $ do
    dropDB dbName'
    createDB dbName'
    importDB (dbFile info) dbName'
