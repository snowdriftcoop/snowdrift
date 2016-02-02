{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE MultiWayIf    #-}

import Prelude hiding (init, length)

import Control.Monad hiding (forM_)
import Control.Monad.Reader (ReaderT, ask, liftIO, runReaderT)
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (toLower)
import Data.Foldable (forM_)
import Data.List.NonEmpty hiding (init)
import Data.Monoid ((<>))
import System.Console.CmdArgs hiding (args)
import System.Directory
import System.Environment
import System.IO
import System.Process
import System.Random.MWC
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

-- Command line interface.

type PgAction = ReaderT Sdm IO

main :: IO ()
main = do
    exists <- doesFileExist "Snowdrift.cabal"
    unless exists $ error "please run from the project's root directory"
    sdm <- getProgName >>= cmdArgs . mkSdm
    run "sudo" ["-K"]  -- require the sudo passphrase each time running sdm
    runReaderT handle sdm

data Sdm = Sdm
    { db       :: String
    , action   :: String
    , pgUser   :: String
    , sudoUser :: String
    } deriving (Typeable, Data, Show)


actions, databases :: String
actions   = "init, clean, reset, export"
databases = "dev, test, all (default)"

mkSdm :: String -> Sdm
mkSdm pname = Sdm
    { db = "all" -- operate on the dev and test databases
        &= help "Database to operate on"
        &= explicit &= name "db"
        &= typ "DATABASE"
    , action = def &= argPos 0 &= typ "ACTION"
    , pgUser = "postgres"
            &= help "Postgres superuser used to create/modify databases"
            &= explicit &= name "pgUser"
    , sudoUser = "postgres"
              &= help "System user used for peer authentication"
              &= explicit &= name "sudoUser"
    } &= summary "Snowdrift database manager 0.1" &= program pname
      &= details [ "Actions: " <> actions
                 , "Databases: " <> databases ]

handle :: PgAction ()
handle = do
    Sdm{..} <- ask
    case action of
        -- Force evaluation to check that the 'db' argument is valid.
        "init"   -> init   $! parse db
        "clean"  -> clean  $! parse db
        "reset"  -> reset  $! parse db
        "export" -> export $! parse db
        _ ->  error $ "invalid action; must be one of: " <> actions

parse :: String -> NonEmpty DB
parse s
    | s == "dev"  = fromList [dev]
    | s == "test" = fromList [test]
    | s == "all"  = fromList [dev, test]
    | otherwise   = error $ "invalid argument to 'db': " <>
                            "must be one of: " <>
                            databases

type Alias = String
data DB = Dev Alias DBInfo | Test Alias DBInfo

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
dev = Dev "dev"
    DBInfo { dbFile = DBFile "devDB.sql"
           , dbUser = DBUser "snowdrift_development"
           , dbName = DBName "snowdrift_development"
           , dbTemp = Nothing }

test = Test "test"
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

sudoed :: String -> [String] -> CreateProcess
sudoed sudoUser args = proc "sudo" $ ["-u", sudoUser] <> args

psql :: String -> PgAction ()
psql cmd = do
    Sdm{..} <- ask
    liftIO $ putStr =<< proc "echo" [cmd] -|-
                        sudoed sudoUser ["psql", "-U", pgUser]

cat :: String -> CreateProcess
cat file = proc "cat" [file]

leave :: String -> IO ()
leave s = do
    pname <- getProgName
    putStr $ pname <> ": " <> s

leaveLn :: String -> IO ()
leaveLn s = leave s >> putStr "\n"

-- Database interaction.
doesDBExist :: DBType a => a -> PgAction Bool
doesDBExist dbType = do
    Sdm{..} <- ask
    dbs <- liftIO $ sudoed sudoUser ["psql", "-lqt", "-U", pgUser] -|-
                    proc "cut" ["-d|", "-f1"]
    return $ elem (toString dbType) (words dbs)

ifExists :: DBType a => a -> PgAction () -> PgAction ()
ifExists dbType as = do
    exists <- doesDBExist dbType
    if | exists    -> as
       | otherwise ->
           liftIO $ leaveLn $ mconcat ["'"
                                      ,toString dbType
                                      ,"' does not exist; doing nothing"]

dropDB, createDB :: DBType a => a -> PgAction ()
dropDB  dbType =
   ifExists dbType (psql $ "DROP DATABASE " <> toString dbType <> ";")

createDB dbType = psql $ "CREATE DATABASE " <> toString dbType <> ";"

importDB :: DBType a => DBFile -> a -> PgAction ()
importDB (DBFile f) dbType = do
    Sdm{..} <- ask
    liftIO $ putStr =<< cat f -|-
                        sudoed sudoUser ["psql", "-U", pgUser, toString dbType]

exportDB :: DBType a => a -> DBFile -> PgAction ()
exportDB dbType (DBFile f) = do
    Sdm{..} <- ask
    liftIO $ loop sudoUser pgUser
  where
    loop sudoUser pgUser = do
        leave $ "overwrite '" <> f <> "'? (yes/No) "
        -- send the question to 'stdout' immediately
        hFlush stdout
        answer <- fmap (fmap toLower) getLine
        if | answer == "yes" -> do
                 (_, Just o, _, _) <-
                   createProcess
                       (sudoed sudoUser ["pg_dump"
                                        ,"-U"
                                        ,pgUser
                                        ,toString dbType]) {
                           std_out = CreatePipe
                       }
                 dump <- hGetContents o
                 writeFile f dump
           | answer == "no" || null answer -> leaveLn "doing nothing"
           | otherwise -> do
                 leaveLn "invalid argument"
                 loop sudoUser pgUser

createUser :: DBUser -> [String] -> PgAction ()
createUser (DBUser u) opts = psql $ mconcat ["CREATE USER "
                                            ,u
                                            ," "
                                            ,unwords opts
                                            ,";"]

dropRole :: DBUser -> PgAction ()
dropRole (DBUser u) = psql $ "DROP ROLE " <> u <> ";"

alterUser :: DBUser -> String -> PgAction ()
alterUser (DBUser u) password = psql $ mconcat ["ALTER USER "
                                               ,u
                                               ," WITH ENCRYPTED PASSWORD '"
                                               ,password
                                               ,"';"]

template :: Bool -> DBTemp -> PgAction ()
template b dbTemp = ifExists dbTemp .
  psql $ mconcat $ ["UPDATE pg_database SET datistemplate="
                   ,show b
                   ," WHERE datname='"
                   ,toString dbTemp
                   ,"';"]

setTemplate, unsetTemplate :: DBTemp -> PgAction ()
setTemplate   = template True
unsetTemplate = template False

-- Actions.

config :: String
config = "config/postgresql.yml"

-- Do not use for important accounts.
genPassword :: Int -> String -> IO String
genPassword len cs =
    C.unpack <$> (withSystemRandom . asGenIO $ go len (return B.empty))
  where
    go 0 acc _   = acc
    go n acc gen = do
        w <- uniformR (c2w ' ', c2w '~') gen
        if (w2c w ==) `any` cs
            then go (pred n) (B.cons w <$> acc) gen
            else go n acc gen

init, clean, reset, export :: NonEmpty DB -> PgAction ()
init ((Test {}) :| []) =
    error "cannot initialize only test; try to init dev or all"
init dbs = do
    exists <- liftIO $ doesFileExist config
    when exists $ error "already initialized; doing nothing"

    password <- liftIO $ genPassword 42 $ ['a'..'z'] <>
                                          ['A'..'Z'] <>
                                          ['0'..'9']

    -- Setup the databases.
    liftIO $ run "cp" ["config/postgresql.template", config]
    forM_ dbs $ \db -> init' db password

    -- sed -i isn't supported on *BSD
    liftIO $ run "perl"
                  ["-pi"
                  ,"-e"
                  ,"s/REPLACE WITH YOUR PASSPHRASE/" <> password <> "/"
                  ,config]
    liftIO $ run "sudo" ["chmod", "400", config]
  where
    init' :: DB -> String -> PgAction ()
    init' (Dev _ (DBInfo {..})) password = do
        createUser dbUser ["NOSUPERUSER", "NOCREATEDB", "NOCREATEROLE"]
        createDB dbName
        alterUser dbUser password
        psql $ mconcat ["GRANT ALL PRIVILEGES ON DATABASE "
                       ,toString dbName
                       ," TO "
                       ,toString dbUser
                       ,";"]
        importDB dbFile dbName

    init' (Test _ (DBInfo {..})) password =
        forM_ dbTemp $ \dbTemp' -> do
            createUser dbUser ["NOSUPERUSER", "CREATEDB", "NOCREATEROLE"]
            createDB dbTemp'
            alterUser dbUser password
            setTemplate dbTemp'
            importDB dbFile dbTemp'

clean dbs
    | length dbs == 1 =
        error "cannot clean a single database; try to reset it or to clean all"
    | otherwise = do
        liftIO $ run "rm" ["-f", config]
        forM_ dbs clean'
  where
    dropDBAndRole db role = dropDB db >> dropRole role
    clean' (Dev _ (DBInfo {..}))  = dropDBAndRole dbName dbUser
    clean' (Test _ (DBInfo {..})) = do
      forM_ dbTemp $ \dbTemp' -> do
          unsetTemplate dbTemp'
          dropDB dbTemp'
      dropDBAndRole dbName dbUser

reset dbs = forM_ dbs reset'
  where
    reset' (Dev _ (DBInfo {..})) = do
        dropDB dbName
        createDB dbName
        importDB dbFile dbName
    reset' (Test _ (DBInfo {..})) =
        forM_ dbTemp $ \dbTemp' -> do
            unsetTemplate dbTemp'
            dropDB dbTemp'
            createDB dbTemp'
            setTemplate dbTemp'
            importDB dbFile dbTemp'

export dbs = forM_ dbs export'
  where
    export' (Dev _ (DBInfo {..}))  = exportDB dbName dbFile
    export' (Test _ (DBInfo {..})) =
        forM_ dbTemp $ \dbTemp' -> exportDB dbTemp' dbFile
