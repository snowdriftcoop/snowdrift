module Handler.User.Users where

import Import

import Data.Time.Format
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Handler.Utils
import Model.Role
import Model.User


getUsersR :: Handler Html
getUsersR = do
    void requireAuth

    (users', allProjects) <- runDB ((,) <$> fetchAllUsersDB
                                        <*> fetchAllUserProjectInfosDB)

    let users :: [(Text, Entity User)]
        users = map (\u -> (getUserKey u :: Text, u)) $ filter isVisible users'

        userProjects :: Entity User -> Maybe (Map (Text, Text) (Set Role))
        userProjects u = M.lookup (entityKey u) allProjects

        getUserKey :: PersistField a => Entity User -> a
        getUserKey = either (error . T.unpack) id .
            fromPersistValue . toPersistValue . entityKey

        isVisible :: Entity User -> Bool
        isVisible = (>= (0::Int)) . getUserKey

    defaultLayout $ do
        snowdriftTitle "Users"
        $(widgetFile "users")
