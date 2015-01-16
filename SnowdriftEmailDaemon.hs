module Main where

import Import hiding (on)
import Model.Notification (NotificationType)
import Model.User         (deleteFromEmailVerification)

import           Control.Concurrent           (threadDelay)
import qualified Control.Exception.Lifted     as Exception
import           Control.Monad.Logger         (runLoggingT, LoggingT, defaultLogStr)
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource (runResourceT, ResourceT)
import           Database.Esqueleto
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Char8        as Char8
import qualified Data.Function                as Function
import           Data.List                    (intercalate, nubBy)
import qualified Database.Persist             as Persist
import           Database.Persist.Postgresql  (PostgresConf)
import qualified Data.Text                    as Text
import qualified Data.Text.Encoding           as Text
import qualified Data.Text.Lazy               as TextLazy
import           Network.Mail.Mime            (simpleMail', Address (..), sendmailCustom, Mail, renderMail')
import           System.Console.CmdArgs
import           System.Directory             (doesFileExist)
import           System.IO                    (stdout, stderr)
import           System.Environment           (getEnv, getProgName)
import           System.Log.FastLogger        (toLogStr, fromLogStr)
import qualified Text.Email.Validate          as Email
import           Yesod.Default.Config         (withYamlEnvironment, DefaultEnv (..))

data Arguments = Arguments
    { db_arg            :: Text
    , email_arg         :: Text
    , delay_arg         :: Int
    , sendmail_exec_arg :: Text
    , sendmail_file_arg :: Text
    } deriving (Typeable, Data, Show)

arguments :: String -> String -> Arguments
arguments pname user = Arguments
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
    , sendmail_exec_arg  = default_sendmail
                        &= help "Location of the sendmail program"
                        &= explicit &= name "sendmail"
                        &= typ "FILE"
    , sendmail_file_arg  = Text.empty
                        &= help "Name of the temporary file (for SnowdriftSendmail)"
                        &= explicit &= name "sendmail-file"
                        &= typ "FILE"
    } &= summary "Snowdrift email daemon 0.1" &= program pname
      &= details ["Databases: " <> (intercalate ", " $ fst <$> databases)]
  where
    default_db       = "development"
    defaultEmail     = (<> "@localhost")
    default_delay    = 10

default_sendmail :: FileName
default_sendmail = "/usr/sbin/sendmail"

databases :: [(String, DefaultEnv)]
databases = [ ("development", Development)
            , ("testing",     Testing)
            , ("staging",     Staging)
            , ("production",  Production) ]

type Email    = Text
type FileName = Text
type Delay    = Int

-- Force evaluation to parse the arguments before doing anything else.
data Parsed = Parsed
    { env           :: !DefaultEnv
    , notif_email   :: !Email
    , loop_delay    :: !Delay
    , sendmail_exec :: !FileName
    , sendmail_file :: !FileName
    }

parse :: Text -> Email -> Int -> FileName -> FileName -> IO Parsed
parse db_arg email_arg delay_arg sendmail_exec_arg sendmail_file_arg = do
    sendmail_exec' <- errUnlessExists $ Text.unpack sendmail_exec_arg
    let sendmail_file_arg' = Text.unpack sendmail_file_arg
    sendmail_file' <- if null sendmail_file_arg'
                          then return sendmail_file_arg'
                          else errUnlessExists sendmail_file_arg'
    return $ Parsed env' notif_email' loop_delay'
        (Text.pack sendmail_exec') (Text.pack sendmail_file')
  where
    env' = case lookup (Text.unpack $ Text.toLower db_arg) databases
           of Nothing -> error $ "unsupported database: "
                              <> Text.unpack db_arg <> "; try '--help'"
              Just v  -> v
    notif_email' =
        if Email.isValid $ Text.encodeUtf8 email_arg
        then email_arg
        else error $ "invalid email address format: " <> Text.unpack email_arg
    loop_delay' =
        if delay_arg < 0
        then error $ "negative delay: " <> show delay_arg
        else delay_arg
    errUnlessExists file = do
        file_exists <- doesFileExist file
        if file_exists
            then return file
            else error $ "file does not exist: " <> file

-- | Select messages to users with verified email addresses.
selectWithVerifiedEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                            [( Maybe Email, UTCTime, NotificationType
                             , UserId, Maybe ProjectId, Markdown )]
selectWithVerifiedEmails =
    (map (\(Value memail, Value ts, Value notif_type, Value to, Value mproject, Value content) ->
           (memail, ts, notif_type, to, mproject, content))) <$>
    (select $
     from $ \(notification_email `InnerJoin` user) -> do
         on $ notification_email ^. NotificationEmailTo ==.
              user ^. UserId
         where_ $ (not_ $ isNothing $ user ^. UserEmail)
              &&. user ^. UserEmail_verified
         return ( user ^. UserEmail
                , notification_email ^. NotificationEmailCreatedTs
                , notification_email ^. NotificationEmailType
                , notification_email ^. NotificationEmailTo
                , notification_email ^. NotificationEmailProject
                , notification_email ^. NotificationEmailContent ))

-- | Select all fields for users without email addresses or verified
-- email addresses such that they could be inserted into the
-- "notification" table without creating duplicates.
selectWithoutEmailsOrVerifiedEmails :: SqlPersistT (ResourceT (LoggingT IO))
                                       [( Value UTCTime
                                        , Value NotificationType
                                        , Value UserId
                                        , Value (Maybe ProjectId)
                                        , Value Markdown )]
selectWithoutEmailsOrVerifiedEmails =
    select $ from $ \(ne, user) -> do
        where_ $ ne ^. NotificationEmailTo ==. user ^. UserId
             &&. (isNothing $ user ^. UserEmail)
             ||. (not_ (isNothing $ user ^. UserEmail) &&.
                  not_ (user ^. UserEmail_verified))
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

insertWithoutEmailsOrVerifiedEmails
    :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
insertWithoutEmailsOrVerifiedEmails = do
    no_emails_or_not_verified <- selectWithoutEmailsOrVerifiedEmails
    forM_ no_emails_or_not_verified $
        \(Value ts, Value notif_type, Value to, Value mproject, Value content) -> do
            insert_ $ Notification ts notif_type to mproject content False
            deleteFromNotificationEmail ts notif_type to mproject content

fromNotificationEmail :: UTCTime -> NotificationType -> UserId
                      -> Maybe ProjectId -> Markdown -> SqlQuery ()
fromNotificationEmail ts notif_type to mproject content =
    from $ \ne -> do
        where_ $ ne ^. NotificationEmailCreatedTs ==. val ts
             &&. ne ^. NotificationEmailType      ==. val notif_type
             &&. ne ^. NotificationEmailTo        ==. val to
             &&. ne ^. NotificationEmailProject `notDistinctFrom` val mproject
             &&. ne ^. NotificationEmailContent   ==. val content

deleteFromNotificationEmail :: MonadIO m => UTCTime -> NotificationType -> UserId
                            -> Maybe ProjectId -> Markdown -> SqlPersistT m ()
deleteFromNotificationEmail ts notif_type to mproject content =
    delete $ fromNotificationEmail ts notif_type to mproject content

insertIntoNotificationEmail :: (MonadIO m, Functor m)
                            => UTCTime -> NotificationType -> UserId
                            -> Maybe ProjectId -> Markdown
                            -> ReaderT SqlBackend m ()
insertIntoNotificationEmail ts notif_type to mproject content = do
    n <- selectCount $ fromNotificationEmail ts notif_type to mproject content
    when (n == 0) $
        insert_ $ NotificationEmail ts notif_type to mproject content

sendmail :: FileName -> FileName -> L.ByteString -> IO ()
sendmail sendmail_exec sendmail_file =
    sendmailCustom (Text.unpack sendmail_exec) $
        if | sendmail_exec == default_sendmail -> ["-t"]
           | Text.null sendmail_file           -> []
           | otherwise                         -> [Text.unpack sendmail_file]

renderSendmail :: FileName -> FileName -> Mail -> IO ()
renderSendmail sendmail_exec sendmail_file =
    sendmail sendmail_exec sendmail_file <=< renderMail'

handleSendmail :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
               => FileName -> FileName -> PostgresConf -> PersistConfigPool PostgresConf
               -> Text -> Email -> Email -> Text -> Text
               -> PersistConfigBackend PostgresConf m () -> Text -> m ()
handleSendmail sendmail_exec sendmail_file dbConf poolConf info_msg from_ to subject body
               action warn_msg = do
    $(logInfo) info_msg
    Exception.handle handler $ do
        liftIO $ renderSendmail sendmail_exec sendmail_file $ simpleMail'
            (Address Nothing to)
            (Address Nothing from_)
            subject
            (TextLazy.fromStrict body)
        runPool dbConf action poolConf
    where
      handler = \(err :: Exception.ErrorCall) -> do
          $(logError) (Text.pack $ show err)
          $(logWarn) warn_msg

sendNotification :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                 => FileName -> FileName -> PostgresConf
                 -> PersistConfigPool PostgresConf -> Email -> Email -> UTCTime
                 -> NotificationType -> UserId -> Maybe ProjectId -> Markdown
                 -> m ()
sendNotification sendmail_exec sendmail_file dbConf poolConf notif_email
                 user_email ts notif_type to mproject content = do
    let content' = unMarkdown content
    handleSendmail sendmail_exec sendmail_file dbConf poolConf
        ("sending a notification to " <> user_email <> "\n" <> content')
        notif_email user_email "Snowdrift.coop notification" content'
        (deleteFromNotificationEmail ts notif_type to mproject content)
        ("sending the notification to " <> user_email <> " failed; " <>
         "will try again later")

selectWithEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                    [(UserId, Maybe Email, Text)]
selectWithEmails =
    fmap (map unwrapValues) $
    select $ from $ \(ev `InnerJoin` u) -> do
        on $ ev ^. EmailVerificationUser ==. u ^. UserId
        where_ $ not_ (isNothing $ u ^. UserEmail)
             &&. u ^. UserEmail ==. (just $ ev ^. EmailVerificationEmail)
             &&. not_ (ev ^. EmailVerificationSent)
        return ( u  ^. UserId
               , u  ^. UserEmail
               , ev ^. EmailVerificationUri )

selectWithoutEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO)) [UserId]
selectWithoutEmails =
    fmap (map (\(Value user_id) -> user_id)) $
    select $ from $ \(ev `InnerJoin` u) -> do
        on $ ev ^. EmailVerificationUser ==. u ^. UserId
        where_ $ isNothing $ u ^. UserEmail
        return $ ev ^. EmailVerificationUser

deleteWithoutEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
deleteWithoutEmails = do
    no_emails <- selectWithoutEmails
    forM_ no_emails $ \user_id ->
        deleteFromEmailVerification user_id

selectWithNonMatchingEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                               [(UserId, Text)]
selectWithNonMatchingEmails =
    fmap (map (\(Value user, Value email) -> (user, email))) $
    select $ from $ \(ev `InnerJoin` u) -> do
        on $ ev ^. EmailVerificationUser ==. u ^. UserId
        where_ $ not_ (isNothing $ u ^. UserEmail)
             &&. u ^. UserEmail !=. just (ev ^. EmailVerificationEmail)
        return ( ev ^. EmailVerificationUser
               , ev ^. EmailVerificationEmail )

deleteWithNonMatchingEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
deleteWithNonMatchingEmails = do
    non_matching <- selectWithNonMatchingEmails
    forM_ non_matching $ \(user, email) ->
        delete $ from $ \ev ->
            where_ $ ev ^. EmailVerificationUser  ==. val user
                 &&. ev ^. EmailVerificationEmail ==. val email

markAsSentVerification :: MonadIO m => UserId -> Email -> Text
                       -> SqlPersistT m ()
markAsSentVerification user_id user_email ver_uri =
    update $ \v -> do
        set v [EmailVerificationSent =. val True]
        where_ $ v ^. EmailVerificationUser  ==. val user_id
             &&. v ^. EmailVerificationEmail ==. val user_email
             &&. v ^. EmailVerificationUri   ==. val ver_uri

sendVerification :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                 => FileName -> FileName -> PostgresConf
                 -> PersistConfigPool PostgresConf -> Email -> UserId -> Email
                 -> Text -> m ()
sendVerification sendmail_exec sendmail_file dbConf poolConf verif_email user_id user_email ver_uri = do
    let content = "Please open this link to verify your email address: "
               <> ver_uri
    handleSendmail sendmail_exec sendmail_file dbConf poolConf
        ("sending an email verification message to " <> user_email <> "\n" <>
         content)
        verif_email user_email "Snowdrift.coop email verification" content
        (markAsSentVerification user_id user_email ver_uri)
        ("sending the email verification message to " <> user_email <>
         " failed; will try again later")

selectUnsentResetPassword :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                             [(UserId, Email, Text)]
selectUnsentResetPassword =
    fmap (distinctFirst . map unwrapValues) $
    select $ from $ \rp -> do
        where_ $ not_ $ rp ^. ResetPasswordSent
        return ( rp ^. ResetPasswordUser
               , rp ^. ResetPasswordEmail
               , rp ^. ResetPasswordUri )
  where
    -- 'nub' is O(n^2).
    distinctFirst = nubBy ((==) `Function.on` (\(x,_,_) -> x))

markAsSentResetPassword :: MonadIO m => UserId -> SqlPersistT m ()
markAsSentResetPassword user_id =
    update $ \rp -> do
        set rp [ResetPasswordSent =. val True]
        where_ $ rp ^. ResetPasswordUser ==. val user_id

sendResetPassword :: ( MonadBaseControl IO m, MonadLogger m, MonadIO m)
                  => FileName -> FileName -> PostgresConf -> ConnectionPool
                  -> Email -> Email -> UserId -> Text -> m ()
sendResetPassword sendmail_exec sendmail_file dbConf poolConf notif_email
                  user_email user_id uri = do
    let content = "Please open this link to set the new password: " <> uri
    handleSendmail sendmail_exec sendmail_file dbConf poolConf
        ("sending a password reset message to " <> user_email <> "\n" <> content)
        notif_email user_email "Snowdrift.coop password reset" content
        (markAsSentResetPassword user_id)
        ("sending the password reset message to " <> user_email <> " failed; " <>
         "will try again later")

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
    Arguments {..} <- liftIO $ cmdArgs $ arguments pname userv
    Parsed {..} <-
        liftIO $ parse db_arg email_arg delay_arg sendmail_exec_arg sendmail_file_arg
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
                with_emails <- selectWithVerifiedEmails
                insertWithoutEmailsOrVerifiedEmails
                return with_emails

        notifs <- runPool dbConf action poolConf
        forM_ notifs $ \(Just user_email, ts, notif_type, to, mproject, content) ->
            sendNotification sendmail_exec sendmail_file dbConf poolConf
                notif_email user_email ts notif_type to mproject content
        let action' = do
                with_emails <- selectWithEmails
                deleteWithNonMatchingEmails
                deleteWithoutEmails
                return with_emails
        verifs <- runPool dbConf action' poolConf
        forM_ verifs $ \(user_id, Just user_email, ver_uri) ->
            sendVerification sendmail_exec sendmail_file dbConf poolConf
                notif_email user_id user_email ver_uri
        resets <- runPool dbConf selectUnsentResetPassword poolConf
        forM_ resets $ \(user_id, user_email, uri) ->
            sendResetPassword sendmail_exec sendmail_file dbConf poolConf
                notif_email user_email user_id uri
