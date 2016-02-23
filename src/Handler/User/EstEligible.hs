module Handler.User.EstEligible where

import Import

import Model.User
import View.User

-- /#UserId/elig

postUserEstEligibleR :: UserId -> Handler Html
postUserEstEligibleR user_id = do
    establisher_id <- requireAuthId

    ok <- canMakeEligible user_id establisher_id
    unless ok $
        error "Error! Maybe user is already eligible or you lack permissions"

    ((result, _), _) <- runFormPost establishUserForm
    case result of
        FormSuccess reason -> do
            user <- runYDB (get404 user_id)
            case userEstablished user of
                EstUnestablished -> do
                    honor_pledge <- getUrlRender >>= \r -> return $ r HonorPledgeR
                    runSDB $ eligEstablishUserDB honor_pledge establisher_id user_id reason
                    setMessage "This user is now eligible for establishment. Thanks!"
                    redirectUltDest HomeR
                _ -> error "User not unestablished!"
        _ -> error "Error submitting form."
