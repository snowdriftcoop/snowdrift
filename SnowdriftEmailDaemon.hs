module Main where

import Import hiding (on, deleteWhere)
import Model.Notification (NotificationType)

import           Control.Concurrent           (threadDelay)
import qualified Control.Exception.Lifted     as Exception
import           Control.Monad.Logger         (runLoggingT, LoggingT, defaultLogStr)
import           Control.Monad.Trans.Resource (runResourceT)
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Sql (unsafeSqlBinOp)
import qualified Data.ByteString.Char8        as Char8
import           Data.List                    (intercalate)
import qualified Database.Persist             as Persist
import           Database.Persist.Postgresql  (PostgresConf)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as TextLazy
import           Network.Mail.Mime            (renderSendMail, simpleMail', Address (..))
import           System.Console.CmdArgs
import           System.IO                    (stdout, stderr)
import           System.Environment           (getEnv, getProgName)
import           System.Log.FastLogger        (toLogStr, fromLogStr)
import qualified Text.Email.Validate          as Email
import           Yesod.Default.Config         (withYamlEnvironment, DefaultEnv (..))
import           Yesod.Markdown               (unMarkdown)

data SnowdriftEmailDaemon = SnowdriftEmailDaemon
    { db_arg    :: Text
    , email_arg :: Text
    , delay_arg :: Int
    } deriving (Typeable, Data, Show)

snowdriftEmailDaemon :: String -> String -> SnowdriftEmailDaemon
snowdriftEmailDaemon pname user = SnowdriftEmailDaemon
    { db_arg    = Text.pack default_db
               &= help ("Database to operate on " <>
                        "(default: " <> default_db <> ")")
               &= explicit &= name "db"
               &= typ "DATABASE"
    , email_arg = Text.pack (defaultEmail user)
               &= help ("Send notifications from this address " <>
                        "(default: " <> defaultEmail user <> ")")
               &= explicit &= name "email"
               &= typ "EMAIL"
    , delay_arg = default_delay
               &= help "Time between each iteration of the loop"
               &= explicit &= name "delay"
               &= typ "SECONDS"
    } &= summary "Snowdrift email daemon 0.1" &= program pname
      &= details ["Databases: " <> (intercalate ", " $ fst <$> databases)]
  where
    default_db    = "development"
    defaultEmail  = (<> "@localhost")
    default_delay = 10

databases :: [(String, DefaultEnv)]
databases = [ ("development", Development)
            , ("testing",     Testing)
            , ("staging",     Staging)
            , ("production",  Production) ]

type Email = Text
type Delay = Int

parse :: Text -> Email -> Int -> (DefaultEnv, Email, Delay)
parse db email delay = (env, notif_email, loop_delay)
  where
    env = case lookup (Text.unpack $ Text.toLower db) databases
          of Nothing -> error $ "unsupported database: "
                             <> Text.unpack db <> "; try '--help'"
             Just v  -> v
    notif_email =
        if Email.isValid $ Text.encodeUtf8 email
        then email
        else error $ "invalid email address format: " <> Text.unpack email
    loop_delay =
        if delay < 0
        then error $ "negative delay: " <> show delay
        else delay

-- | Select messages to users with email addresses.
selectWithEmails :: (MonadResource m, MonadSqlPersist m)
                 => m [( Maybe Email, UTCTime, NotificationType
                       , UserId, Maybe ProjectId, Markdown )]
selectWithEmails =
    (map (\(Value memail, Value ts, Value notif_type, Value to, Value mproject, Value content) ->
           (memail, ts, notif_type, to, mproject, content))) <$>
    (select $
     from $ \(notification_email `InnerJoin` user) -> do
         on $ notification_email ^. NotificationEmailTo ==.
              user ^. UserId
         where_ $ not_ $ isNothing $ user ^. UserEmail
         return ( user ^. UserEmail
                , notification_email ^. NotificationEmailCreatedTs
                , notification_email ^. NotificationEmailType
                , notification_email ^. NotificationEmailTo
                , notification_email ^. NotificationEmailProject
                , notification_email ^. NotificationEmailContent ))

infix 4 `notDistinctFrom`
notDistinctFrom :: SqlExpr (Value a) -> SqlExpr (Value a)
                -> SqlExpr (Value Bool)
notDistinctFrom = unsafeSqlBinOp " IS NOT DISTINCT FROM "

--- | Select all fields for users without email addresses such that
--- they could be inserted into the "notification" table without
--- creating duplicates.
selectWithoutEmails :: (MonadResource m, MonadSqlPersist m)
                    => m [( Value UTCTime
                          , Value NotificationType
                          , Value UserId
                          , Value (Maybe ProjectId)
                          , Value Markdown )]
selectWithoutEmails =
    select $ from $ \(ne, user) -> do
        where_ $ ne ^. NotificationEmailTo ==. user ^. UserId
             &&. (isNothing $ user ^. UserEmail)
             &&. ne ^. NotificationEmailTo
             `notIn` (subList_select $ from $ \n -> do
                          where_ $ n  ^. NotificationType
                               ==. ne ^. NotificationEmailType
                               &&. n  ^. NotificationTo
                               ==. ne ^. NotificationEmailTo
                               &&. n  ^. NotificationProject `notDistinctFrom`
                                   ne ^. NotificationEmailProject
                               &&. n  ^. NotificationContent
                               ==. ne ^. NotificationEmailContent
                          return (ne ^. NotificationEmailTo))
        return ( ne ^. NotificationEmailCreatedTs
               , ne ^. NotificationEmailType
               , ne ^. NotificationEmailTo
               , ne ^. NotificationEmailProject
               , ne ^. NotificationEmailContent )

insertWithoutEmails :: ( MonadResource m, PersistMonadBackend m ~ SqlBackend
                       , PersistStore m, MonadSqlPersist m ) => m ()
insertWithoutEmails = do
    no_emails <- selectWithoutEmails
    forM_ no_emails $ \( Value ts, Value notif_type, Value to
                       , Value mproject, Value content ) -> do
        insert_ $ Notification ts notif_type to mproject content False
        deleteWhere ts notif_type to mproject content

fromNotificationEmail :: UTCTime -> NotificationType -> UserId
                      -> Maybe ProjectId -> Markdown -> SqlQuery ()
fromNotificationEmail ts notif_type to mproject content =
    from $ \ne -> do
        where_ $ ne ^. NotificationEmailCreatedTs ==. val ts
             &&. ne ^. NotificationEmailType      ==. val notif_type
             &&. ne ^. NotificationEmailTo        ==. val to
             &&. ne ^. NotificationEmailProject `notDistinctFrom` val mproject
             &&. ne ^. NotificationEmailContent   ==. val content

deleteWhere :: (MonadResource m, MonadSqlPersist m)
            => UTCTime -> NotificationType -> UserId -> Maybe ProjectId
            -> Markdown -> m ()
deleteWhere ts notif_type to mproject content =
    delete $ fromNotificationEmail ts notif_type to mproject content

insertWhere :: ( MonadResource m, PersistStore m, MonadSqlPersist m
               , PersistMonadBackend m ~ SqlBackend)
            => UTCTime -> NotificationType -> UserId -> Maybe ProjectId
            -> Markdown -> m ()
insertWhere ts notif_type to mproject content = do
    n <- fmap (\[Value n] -> n :: Int) $
         select $ fromNotificationEmail ts notif_type to mproject content
               >> return countRows
    if n == 0
        then insert_ $ NotificationEmail ts notif_type to mproject content
        else return ()

sendNotif :: (MonadResource m, MonadBaseControl IO m, MonadIO m, MonadLogger m)
          => PostgresConf -> PersistConfigPool PostgresConf
          -> Email -> Email -> UTCTime -> NotificationType -> UserId
          -> Maybe ProjectId -> Markdown -> m ()
sendNotif  dbConf poolConf notif_email user_email ts notif_type to mproject content =
    Exception.handle handler $ do
        let content' = unMarkdown content
        $(logInfo) ("sending a notification to " <> user_email <>
                    "\n" <> content')
        liftIO $ renderSendMail $ simpleMail'
            (Address Nothing user_email)
            (Address Nothing notif_email)
            "Snowdrift.coop notification"
            (TextLazy.fromStrict content')
        let action = deleteWhere ts notif_type to mproject content
        runPool dbConf action poolConf
    where
      handler = \(err :: Exception.ErrorCall) -> do
          $(logError) (Text.pack $ show err)
          $(logWarn) ("sending the notification to " <> user_email <> " failed\n" <>
                      "re-inserting data into the \"notification_email\" table")
          let action = insertWhere ts notif_type to mproject content
          runPool dbConf action poolConf

withLogging :: MonadIO m => LoggingT m a -> m a
withLogging m = runLoggingT m $ \loc src level str ->
    let out = if level == LevelError then stderr else stdout
    in Char8.hPutStrLn out $
           fromLogStr $ defaultLogStr loc src level $ toLogStr str

withDelay :: MonadIO m => Delay -> m a -> m ()
withDelay delay action = action >> (liftIO $ threadDelay $ 1000000 * delay)

main :: IO ()
main = withLogging $ do
    pname <- liftIO $ getProgName
    userv <- liftIO $ getEnv "USER"
    SnowdriftEmailDaemon {..} <- liftIO $ cmdArgs $ snowdriftEmailDaemon pname userv
    -- Force evaluation to check the arguments first.
    let !(!env, !notif_email, !loop_delay) = parse db_arg email_arg delay_arg
    $(logInfo) "starting the daemon"
    $(logDebug) ("running with --db=" <> db_arg <> " --email=" <> notif_email <>
                 " --delay=" <> (Text.pack $ show loop_delay))
    (dbConf, poolConf) <- liftIO $ do
        dbConf   <- withYamlEnvironment "config/postgresql.yml" env
                        Persist.loadConfig >>= Persist.applyEnv :: IO PostgresConf
        poolConf <- Persist.createPoolConfig dbConf
        return (dbConf, poolConf)
    $(logInfo) "starting the main loop"
    void $ forever $ runResourceT $ withDelay loop_delay $ do
        let action = do
                emails <- selectWithEmails
                insertWithoutEmails
                return emails
        notifs <- runPool dbConf action poolConf
        forM_ notifs $ \(Just user_email, ts, notif_type, to, mproject, content) ->
            sendNotif dbConf poolConf notif_email user_email ts notif_type to mproject content
