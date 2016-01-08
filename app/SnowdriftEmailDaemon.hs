module Main where

import Import hiding (on)

import Control.Concurrent (threadDelay)
import Control.Monad.Logger (runLoggingT, LoggingT, defaultLogStr)
import Control.Monad.Reader
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.List (intercalate, nubBy)
import Database.Esqueleto
import Database.Persist.Postgresql (PostgresConf)
import Network.Mail.Mime
            (simpleMail', Address (..), sendmailCustom, Mail, renderMail')
import System.Console.CmdArgs
import System.Directory (doesFileExist)
import System.Environment (getEnv, getProgName)
import System.IO (hFlush, stdout, stderr)
import System.Log.FastLogger (toLogStr, fromLogStr)
import Yesod.Default.Config (withYamlEnvironment, DefaultEnv (..))
import qualified Control.Exception.Lifted as Exception
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as L
import qualified Data.Function as Function
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Database.Persist as Persist
import qualified Text.Email.Validate as Email

import Model.User (deleteFromEmailVerification)
import WrappedValues

data Arguments = Arguments
    { db_arg            :: Text
    , email_arg         :: Text
    , delay_arg         :: Int
    , sendmail_command_arg :: Text
    , sendmail_file_arg :: Text
    } deriving (Typeable, Data, Show)

data Command = Command
    { theCommand :: !Text
    , commandArgs :: ![Text]
    }

arguments :: String -> String -> Arguments
arguments pname user = Arguments
    { db_arg = Text.pack default_db
        &= help ("Database to operate on " <>
                 "(default: " <> default_db <> ")")
        &= explicit
        &= name "db"
        &= typ "DATABASE"
    , email_arg = Text.pack (defaultEmail user)
        &= help ("Send notifications from this address " <>
                 "(default: " <> defaultEmail user <> ")")
        &= explicit
        &= name "email"
        &= typ "EMAIL"
    , delay_arg = default_delay
        &= help "Time between each iteration of the loop"
        &= explicit
        &= name "delay"
        &= typ "SECONDS"
    , sendmail_command_arg = default_sendmail
        &= help "Command line to run the sendmail program"
        &= explicit
        &= name "sendmail"
        &= typ "FILE"
    , sendmail_file_arg  = Text.empty
        &= help "Name of the temporary file (for SnowdriftSendmail)"
        &= explicit
        &= name "sendmail-file"
        &= typ "FILE"
    } &= summary "Snowdrift email daemon 0.1"
      &= program pname
      &= details ["Databases: " <> intercalate ", " (fst <$> databases)]
  where
    default_db       = "development"
    defaultEmail     = (<> "@localhost")
    default_delay    = 10

default_sendmail :: Text
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
    , sendmail_command :: !Command
    , sendmail_file :: !FileName
    }

parse :: Arguments -> IO Parsed
parse Arguments{..} = do
    let sendmail_file_arg' = Text.unpack sendmail_file_arg
    sendmail_file' <- if null sendmail_file_arg'
                          then return sendmail_file_arg'
                          else errUnlessExists sendmail_file_arg'
    return $ Parsed env'
                    notif_email'
                    loop_delay'
                    (parseCommand sendmail_command_arg)
                    (Text.pack sendmail_file')
  where
    parseCommand cmdStr =
        if Text.any (== '\\') cmdStr
        then error "Backslashes within command strings are not supported"
        else case Text.splitOn " " cmdStr of
            h:t -> Command h t
            _ -> error "You asked to specify a sendmail command, but didn't supply one"
    env' = fromMaybe
        (error $ "unsupported database: "
                  <> Text.unpack db_arg <> "; try '--help'")
        (lookup (Text.unpack $ Text.toLower db_arg) databases)
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

selectWithVerifiedEmailsUser :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                                [( Maybe Email
                                 , UTCTime
                                 , UserNotificationType
                                 , UserId
                                 , Markdown )]
selectWithVerifiedEmailsUser = fmap unwrapValues $
    select $
    from $ \(email `InnerJoin` user) -> do
    on $
        email ^. UserNotificationEmailTo ==. user ^. UserId
    where_ $
        (not_ $ isNothing $ user ^. UserEmail)
        &&. user ^. UserEmail_verified
    return
        ( user ^. UserEmail
        , email ^. UserNotificationEmailCreatedTs
        , email ^. UserNotificationEmailType
        , email ^. UserNotificationEmailTo
        , email ^. UserNotificationEmailContent
        )

selectWithVerifiedEmailsProject :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                                   [( Maybe Email
                                    , UTCTime
                                    , ProjectNotificationType
                                    , UserId
                                    , ProjectId
                                    , Markdown )]
selectWithVerifiedEmailsProject = fmap unwrapValues $
    select $
    from $ \(email `InnerJoin` user) -> do
    on $ email ^. ProjectNotificationEmailTo ==. user ^. UserId
    where_ $ not_ (isNothing $ user ^. UserEmail)
             &&. user ^. UserEmail_verified
    return ( user ^. UserEmail
           , email ^. ProjectNotificationEmailCreatedTs
           , email ^. ProjectNotificationEmailType
           , email ^. ProjectNotificationEmailTo
           , email ^. ProjectNotificationEmailProject
           , email ^. ProjectNotificationEmailContent )

selectWithoutEmailsOrVerifiedEmailsUser :: (    SqlExpr (Value UserId)
                                             -> SqlExpr (ValueList UserId)
                                             -> SqlExpr (Value Bool) )
                                        -> SqlPersistT (ResourceT (LoggingT IO))
                                           [( Value UTCTime
                                            , Value UserNotificationType
                                            , Value UserId
                                            , Value Markdown )]
selectWithoutEmailsOrVerifiedEmailsUser inOrNotIn =
    select $
    from $ \(ne, user) -> do
    where_ $ (     ne ^. UserNotificationEmailTo ==. user ^. UserId
               &&. (     isNothing (user ^. UserEmail)
                     ||. not_ (isNothing $ user ^. UserEmail)
                     &&. not_ (user ^. UserEmail_verified)
                   )
             ) &&. ne ^. UserNotificationEmailTo `inOrNotIn`
                   (subList_select $ from $ \n -> do
                         where_ $ n  ^. UserNotificationType
                              ==. ne ^. UserNotificationEmailType
                              &&. n  ^. UserNotificationTo
                              ==. ne ^. UserNotificationEmailTo
                              &&. n  ^. UserNotificationContent
                              ==. ne ^. UserNotificationEmailContent
                         return $ ne ^. UserNotificationEmailTo)
    return ( ne ^. UserNotificationEmailCreatedTs
           , ne ^. UserNotificationEmailType
           , ne ^. UserNotificationEmailTo
           , ne ^. UserNotificationEmailContent )

selectWithoutEmailsOrVerifiedEmailsProject :: (    SqlExpr (Value UserId)
                                                -> SqlExpr (ValueList UserId)
                                                -> SqlExpr (Value Bool) )
                                           -> SqlPersistT (ResourceT (LoggingT IO))
                                              [( Value UTCTime
                                               , Value ProjectNotificationType
                                               , Value UserId
                                               , Value ProjectId
                                               , Value Markdown )]
selectWithoutEmailsOrVerifiedEmailsProject inOrNotIn =
    select $ from $ \(ne, user) -> do
    where_ $ (     ne ^. ProjectNotificationEmailTo ==. user ^. UserId
               &&. (     isNothing (user ^. UserEmail)
                     ||. not_ (isNothing $ user ^. UserEmail)
                     &&. not_ (user ^. UserEmail_verified)
                   )
             ) &&. ne ^. ProjectNotificationEmailTo `inOrNotIn`
                   (subList_select $ from $ \n -> do
                         where_ $ n  ^. ProjectNotificationType
                              ==. ne ^. ProjectNotificationEmailType
                              &&. n  ^. ProjectNotificationTo
                              ==. ne ^. ProjectNotificationEmailTo
                              &&. n  ^. ProjectNotificationProject
                              ==. ne ^. ProjectNotificationEmailProject
                              &&. n  ^. ProjectNotificationContent
                              ==. ne ^. ProjectNotificationEmailContent
                         return $ ne ^. ProjectNotificationEmailTo)
    return ( ne ^. ProjectNotificationEmailCreatedTs
           , ne ^. ProjectNotificationEmailType
           , ne ^. ProjectNotificationEmailTo
           , ne ^. ProjectNotificationEmailProject
           , ne ^. ProjectNotificationEmailContent )

-- | Select all fields for users without email addresses or verified
-- email addresses such that they could be inserted into the
-- "notification" table without creating duplicates.
selectUniqueWithoutEmailsOrVerifiedEmailsUser
    -- | Select all fields for users without email addresses or verified
    -- email addresses, which appear in both the "notification" and
    -- "notification_email" tables, so they could be deleted from the
    -- latter.
    , selectDuplicatesWithoutEmailsOrVerifiedEmailsUser
    :: SqlPersistT (ResourceT (LoggingT IO))
       [( Value UTCTime
        , Value UserNotificationType
        , Value UserId
        , Value Markdown )]
selectUniqueWithoutEmailsOrVerifiedEmailsUser =
    selectWithoutEmailsOrVerifiedEmailsUser notIn
selectDuplicatesWithoutEmailsOrVerifiedEmailsUser =
    selectWithoutEmailsOrVerifiedEmailsUser in_

selectUniqueWithoutEmailsOrVerifiedEmailsProject
    , selectDuplicatesWithoutEmailsOrVerifiedEmailsProject
    :: SqlPersistT (ResourceT (LoggingT IO))
       [( Value UTCTime
        , Value ProjectNotificationType
        , Value UserId
        , Value ProjectId
        , Value Markdown )]
selectUniqueWithoutEmailsOrVerifiedEmailsProject =
    selectWithoutEmailsOrVerifiedEmailsProject notIn
selectDuplicatesWithoutEmailsOrVerifiedEmailsProject =
    selectWithoutEmailsOrVerifiedEmailsProject in_

insertWithoutEmailsOrVerifiedEmailsUser
    , insertWithoutEmailsOrVerifiedEmailsProject
    :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
insertWithoutEmailsOrVerifiedEmailsUser = do
    no_emails_or_not_verified <- selectUniqueWithoutEmailsOrVerifiedEmailsUser
    forM_ no_emails_or_not_verified $
        \(Value ts, Value notif_type, Value to, Value content) -> do
            insert_ $ UserNotification ts notif_type to content False
            deleteFromUserNotificationEmail ts notif_type to content
insertWithoutEmailsOrVerifiedEmailsProject = do
    no_emails_or_not_verified <- selectUniqueWithoutEmailsOrVerifiedEmailsProject
    forM_ no_emails_or_not_verified $
        \(Value ts, Value notif_type, Value to, Value project, Value content) -> do
            insert_ $ ProjectNotification ts notif_type to project content False
            deleteFromProjectNotificationEmail ts notif_type to project content

deleteDuplicatesWithoutEmailsOrVerifiedEmailsUser
    , deleteDuplicatesWithoutEmailsOrVerifiedEmailsProject
    :: ReaderT SqlBackend (ResourceT (LoggingT IO)) ()
deleteDuplicatesWithoutEmailsOrVerifiedEmailsUser = do
    duplicates <- selectDuplicatesWithoutEmailsOrVerifiedEmailsUser
    forM_ duplicates $
        \(Value ts, Value notif_type, Value to, Value content) ->
            deleteFromUserNotificationEmail ts notif_type to content
deleteDuplicatesWithoutEmailsOrVerifiedEmailsProject = do
    duplicates <- selectDuplicatesWithoutEmailsOrVerifiedEmailsProject
    forM_ duplicates $
        \(Value ts, Value notif_type, Value to, Value project, Value content) ->
            deleteFromProjectNotificationEmail ts notif_type to project content

fromUserNotificationEmail :: UTCTime
                          -> UserNotificationType
                          -> UserId
                          -> Markdown
                          -> SqlQuery ()
fromUserNotificationEmail ts notif_type to content = from $ \ne ->
    where_ $ ne ^. UserNotificationEmailCreatedTs ==. val ts
         &&. ne ^. UserNotificationEmailType      ==. val notif_type
         &&. ne ^. UserNotificationEmailTo        ==. val to
         &&. ne ^. UserNotificationEmailContent   ==. val content

fromProjectNotificationEmail :: UTCTime
                             -> ProjectNotificationType
                             -> UserId
                             -> ProjectId
                             -> Markdown
                             -> SqlQuery ()
fromProjectNotificationEmail ts notif_type to project content = from $ \ne ->
    where_ $ ne ^. ProjectNotificationEmailCreatedTs ==. val ts
         &&. ne ^. ProjectNotificationEmailType      ==. val notif_type
         &&. ne ^. ProjectNotificationEmailTo        ==. val to
         &&. ne ^. ProjectNotificationEmailProject   ==. val project
         &&. ne ^. ProjectNotificationEmailContent   ==. val content

deleteFromUserNotificationEmail :: MonadIO m
                                => UTCTime
                                -> UserNotificationType
                                -> UserId
                                -> Markdown
                                -> SqlPersistT m ()
deleteFromUserNotificationEmail ts notif_type to content =
    delete $ fromUserNotificationEmail ts notif_type to content

deleteFromProjectNotificationEmail :: MonadIO m
                                   => UTCTime
                                   -> ProjectNotificationType
                                   -> UserId
                                   -> ProjectId
                                   -> Markdown
                                   -> SqlPersistT m ()
deleteFromProjectNotificationEmail ts notif_type to project content =
    delete $ fromProjectNotificationEmail ts notif_type to project content

sendmail :: Command -> FileName -> L.ByteString -> IO ()
sendmail Command{..} sendmail_file =
    sendmailCustom (Text.unpack theCommand) $ (map Text.unpack commandArgs) ++
        if | theCommand == default_sendmail -> ["-t"]
           | Text.null sendmail_file        -> []
           | otherwise                      -> [Text.unpack sendmail_file]

renderSendmail :: Command -> FileName -> Mail -> IO ()
renderSendmail sendmail_command sendmail_file =
    sendmail sendmail_command sendmail_file <=< renderMail'

runSql :: (MonadBaseControl IO m, MonadIO m)
       => PostgresConf
       -> PersistConfigPool PostgresConf
       -> PersistConfigBackend PostgresConf m a
       -> m a
runSql dbConf poolConf action = runPool dbConf action poolConf

handleSendmail :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
               => Command
               -> FileName
               -> PostgresConf
               -> PersistConfigPool PostgresConf
               -> Text
               -> Text
               -> Email
               -> Email
               -> Text
               -> Text
               -> PersistConfigBackend PostgresConf m ()
               -> Text
               -> m ()
handleSendmail
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        info_msg
        info_msg_after
        from_
        to
        subject
        body
        action
        warn_msg = do
    $(logInfo) info_msg
    liftIO $ hFlush stdout
    Exception.handle handler $ do
        liftIO $ renderSendmail sendmail_command sendmail_file $ simpleMail'
            (Address Nothing to)
            (Address Nothing from_)
            subject
            (TextLazy.fromStrict body)
        runSql dbConf poolConf action
    $(logInfo) info_msg_after
    liftIO $ hFlush stdout
    where
      handler (err :: Exception.ErrorCall) = do
          $(logError) (Text.pack $ show err)
          $(logWarn) warn_msg

sendUserNotification :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                     => Command
                     -> FileName
                     -> PostgresConf
                     -> PersistConfigPool PostgresConf
                     -> Email
                     -> Email
                     -> UTCTime
                     -> UserNotificationType
                     -> UserId
                     -> Markdown
                     -> m ()
sendUserNotification
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        notif_email
        user_email
        ts
        notif_type
        to
        content = do
    let content' = unMarkdown content
    handleSendmail sendmail_command sendmail_file dbConf poolConf
        ("sending a user notification to " <> user_email <> "\n" <> content')
        ("sent the user notification to " <> user_email <> "\n" <> content')
        notif_email user_email "Snowdrift.coop notification" content'
        (deleteFromUserNotificationEmail ts notif_type to content)
        ("sending the user notification to " <> user_email <> " failed; " <>
         "will try again later")

sendProjectNotification :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                        => Command
                        -> FileName
                        -> PostgresConf
                        -> PersistConfigPool PostgresConf
                        -> Email
                        -> Email
                        -> UTCTime
                        -> ProjectNotificationType
                        -> UserId
                        -> ProjectId
                        -> Markdown
                        -> m ()
sendProjectNotification
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        notif_email
        user_email
        ts
        notif_type
        to
        project
        content = do
    let content' = unMarkdown content
    handleSendmail sendmail_command sendmail_file dbConf poolConf
        ("sending a project notification to " <> user_email <> "\n" <> content')
        ("sent the project notification to " <> user_email <> "\n" <> content')
        notif_email user_email "Snowdrift.coop notification" content'
        (deleteFromProjectNotificationEmail ts notif_type to project content)
        ("sending the project notification to " <> user_email <> " failed; " <>
         "will try again later")

selectWithEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                    [(UserId, Maybe Email, Text)]
selectWithEmails = fmap unwrapValues $
    select $ from $ \(ev `InnerJoin` u) -> do
    on $ ev ^. EmailVerificationUser ==. u ^. UserId
    where_ $ not_ (isNothing $ u ^. UserEmail)
         &&. u ^. UserEmail ==. just (ev ^. EmailVerificationEmail)
         &&. not_ (ev ^. EmailVerificationSent)
    return ( u  ^. UserId
           , u  ^. UserEmail
           , ev ^. EmailVerificationUri )

selectWithoutEmails :: ReaderT SqlBackend (ResourceT (LoggingT IO)) [UserId]
selectWithoutEmails = fmap unwrapValues $
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
selectWithNonMatchingEmails = fmap unwrapValues $
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
        delete $
        from $ \ev ->
        where_ $ ev ^. EmailVerificationUser  ==. val user
            &&. ev ^. EmailVerificationEmail ==. val email

markAsSentVerification :: MonadIO m
                       => UserId
                       -> Email
                       -> Text
                       -> SqlPersistT m ()
markAsSentVerification user_id user_email ver_uri =
    update $ \v -> do
    set v [EmailVerificationSent =. val True]
    where_ $ v ^. EmailVerificationUser  ==. val user_id
        &&. v ^. EmailVerificationEmail ==. val user_email
        &&. v ^. EmailVerificationUri   ==. val ver_uri

sendVerification :: (MonadBaseControl IO m, MonadIO m, MonadLogger m)
                 => Command
                 -> FileName
                 -> PostgresConf
                 -> PersistConfigPool PostgresConf
                 -> Email
                 -> UserId
                 -> Email
                 -> Text
                 -> m ()
sendVerification
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        verif_email
        user_id
        user_email
        ver_uri = do
    let content = "Please open this link to verify your email address: "
               <> ver_uri
    handleSendmail
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        ("sending an email verification message to " <> user_email <> "\n" <>
         content)
        ("sent the email verification message to " <> user_email <> "\n" <>
         content)
        verif_email
        user_email
        "Snowdrift.coop email verification"
        content
        (markAsSentVerification user_id user_email ver_uri)
        ("sending the email verification message to " <> user_email <>
         " failed; will try again later")

selectUnsentResetPassphrase :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                             [(UserId, Email, Text)]
selectUnsentResetPassphrase = fmap (distinctFirst . unwrapValues) $
    select $ from $ \rp -> do
    where_ $ not_ $ rp ^. ResetPassphraseSent
    return ( rp ^. ResetPassphraseUser
           , rp ^. ResetPassphraseEmail
           , rp ^. ResetPassphraseUri )

-- XXX: 'nub' is O(n^2).
distinctFirst :: [(UserId, a, b)] -> [(UserId, a, b)]
distinctFirst = nubBy ((==) `Function.on` (\(x,_,_) -> x))

markAsSentResetPassphrase :: MonadIO m => UserId -> SqlPersistT m ()
markAsSentResetPassphrase user_id =
    update $ \rp -> do
    set rp [ResetPassphraseSent =. val True]
    where_ $ rp ^. ResetPassphraseUser ==. val user_id

sendResetPassphrase :: ( MonadBaseControl IO m, MonadLogger m, MonadIO m)
                  => Command
                  -> FileName
                  -> PostgresConf
                  -> ConnectionPool
                  -> Email
                  -> Email
                  -> UserId
                  -> Text
                  -> m ()
sendResetPassphrase
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        notif_email
        user_email
        user_id
        uri = do
    let content = "Please open this link to set the new passphrase: " <> uri
    handleSendmail
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        ("sending a passphrase reset message to " <> user_email <> "\n" <> content)
        ("sent the passphrase reset message to " <> user_email <> "\n" <> content)
        notif_email
        user_email
        "Snowdrift.coop passphrase reset"
        content
        (markAsSentResetPassphrase user_id)
        ("sending the passphrase reset message to " <> user_email <> " failed; " <>
         "will try again later")

selectUnsentDeleteConfirmation :: ReaderT SqlBackend (ResourceT (LoggingT IO))
                                  [(UserId, Email, Text)]
selectUnsentDeleteConfirmation = fmap (distinctFirst . map unwrapValues) $
    select $ from $ \dc -> do
    where_ $ not_ $ dc ^. DeleteConfirmationSent
    return ( dc ^. DeleteConfirmationUser
           , dc ^. DeleteConfirmationEmail
           , dc ^. DeleteConfirmationUri )

markAsSentDeleteConfirmation :: MonadIO m => UserId -> SqlPersistT m ()
markAsSentDeleteConfirmation user_id =
    update $ \dc -> do
    set dc [DeleteConfirmationSent =. val True]
    where_ $ dc ^. DeleteConfirmationUser ==. val user_id

sendDeleteConfirmation :: ( MonadBaseControl IO m, MonadLogger m, MonadIO m)
                       => Command
                       -> FileName
                       -> PostgresConf
                       -> ConnectionPool
                       -> Email
                       -> UserId
                       -> Email
                       -> Text
                       -> m ()
sendDeleteConfirmation
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        notif_email
        user_id
        user_email
        uri = do
    let content = "Please open this link to delete your account: " <> uri
    handleSendmail
        sendmail_command
        sendmail_file
        dbConf
        poolConf
        ("sending a delete confirmation message to " <> user_email <> "\n" <>
         content)
        ("sent the delete confirmation message to " <> user_email <> "\n" <>
         content)
        notif_email
        user_email
        "Snowdrift.coop delete confirmation"
        content
        (markAsSentDeleteConfirmation user_id)
        ("sending the delete confirmation message to " <> user_email <> " failed; " <>
         "will try again later")

withLogging :: MonadIO m => LoggingT m a -> m a
withLogging m = runLoggingT m $ \loc src level str ->
    let out = if level == LevelError then stderr else stdout
    in Char8.hPutStrLn out
                       (fromLogStr (defaultLogStr loc src level (toLogStr str)))

withDelay :: MonadIO m => Delay -> m a -> m ()
withDelay delay action = action >> liftIO (threadDelay $ 1000000 * delay)

main :: IO ()
main = withLogging $ do
    pname <- liftIO getProgName
    userv <- liftIO $ getEnv "USER"
    args' <- liftIO $ cmdArgs $ arguments pname userv
    Parsed {..} <-
        liftIO $ parse args'
    $(logInfo) "starting the daemon"
    liftIO $ hFlush stdout
    $(logDebug) ("running with --db=" <> (db_arg args') <> " --email=" <> notif_email <>
                 " --delay=" <> Text.pack (show loop_delay))
    (dbConf, poolConf) <- liftIO $ do
        dbConf   <- withYamlEnvironment "config/postgresql.yml" env
                        Persist.loadConfig >>= Persist.applyEnv :: IO PostgresConf
        poolConf <- Persist.createPoolConfig dbConf
        return (dbConf, poolConf)
    $(logInfo) "starting the main loop"
    liftIO $ hFlush stdout
    void $ forever $ runResourceT $ withDelay loop_delay $ do
        user_notifs <- runSql dbConf poolConf $ do
            with_emails_user <- selectWithVerifiedEmailsUser
            deleteDuplicatesWithoutEmailsOrVerifiedEmailsUser
            insertWithoutEmailsOrVerifiedEmailsUser
            return with_emails_user
        forM_ user_notifs $ \(Just user_email, ts, notif_type, to, content) ->
            sendUserNotification sendmail_command
                                 sendmail_file
                                 dbConf
                                 poolConf
                                 notif_email
                                 user_email
                                 ts
                                 notif_type
                                 to
                                 content

        project_notifs <- runSql dbConf poolConf $ do
            with_emails_project <- selectWithVerifiedEmailsProject
            deleteDuplicatesWithoutEmailsOrVerifiedEmailsProject
            insertWithoutEmailsOrVerifiedEmailsProject
            return with_emails_project
        forM_ project_notifs $ \(Just user_email, ts, notif_type, to, project, content) ->
            sendProjectNotification sendmail_command
                                    sendmail_file
                                    dbConf
                                    poolConf
                                    notif_email
                                    user_email
                                    ts
                                    notif_type
                                    to
                                    project content

        verifs <- runSql dbConf poolConf $ do
            with_emails <- selectWithEmails
            deleteWithNonMatchingEmails
            deleteWithoutEmails
            return with_emails
        forM_ verifs $ \(user_id, Just user_email, ver_uri) ->
            sendVerification sendmail_command
                             sendmail_file
                             dbConf
                             poolConf
                             notif_email
                             user_id
                             user_email
                             ver_uri

        resets <- runSql dbConf poolConf selectUnsentResetPassphrase
        forM_ resets $ \(user_id, user_email, uri) ->
            sendResetPassphrase sendmail_command
                                sendmail_file dbConf poolConf
                                notif_email
                                user_email
                                user_id
                                uri

        confirms <- runSql dbConf poolConf selectUnsentDeleteConfirmation
        forM_ confirms $ \(user_id, user_email, uri) ->
            sendDeleteConfirmation sendmail_command
                                   sendmail_file
                                   dbConf
                                   poolConf
                                   notif_email
                                   user_id
                                   user_email
                                   uri
        $(logInfo) "iteration finished"
        liftIO $ hFlush stdout
