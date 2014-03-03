
module Widgets.Navbar where

import Import

import Model.Currency
import Model.User
import Model.Project

import qualified Data.List as L

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth
    maybe_route <- handlerToWidget getCurrentRoute

    alreadyExpired

    role_values <- case maybe_user of
        Nothing -> return []
        Just (Entity user_id _) -> handlerToWidget $ runDB $ select $ from $ \ project_user_role -> do
            where_ $ project_user_role ^. ProjectUserRoleUser ==. val user_id
            return $ project_user_role ^. ProjectUserRoleRole

    let is_committee_member = False -- TODO

        string :: String -> String
        string = id
        sb_link route name = do
            authorized <- handlerToWidget $ isAuthorized route False
            case authorized of
             Authorized -> [whamlet|
                    $if maybe_route == Just route
                        #{string name}
                    $else
                        <a href=@{route}>
                            #{string name}
                    <br>
                |]

             _ -> return ()

    money_info <- case maybe_user of
        Nothing -> return Nothing
        Just (Entity user_id user) -> do
            (pledges, balance) <- handlerToWidget $ runDB $ do
                pledges :: [(Entity Project, Entity Pledge)] <- select $ from $ \ (project `InnerJoin` pledge) -> do
                    on_ $ pledge ^. PledgeProject ==. project ^. ProjectId
                    where_ $ pledge ^. PledgeUser ==. val user_id
                    return (project, pledge)

                Just account <- get (userAccount user)
                return (pledges, accountBalance account)
            let pledged = sum $ map (\ (project, pledge) -> ((projectShareValue (entityVal project) $*) . fromIntegral . pledgeFundedShares . entityVal) pledge) pledges
            return $ Just (balance, pledged)



    (messages, applications, edits, comments) <- case maybe_user of
        Nothing -> return ([], [], 0, 0)
        Just (Entity user_id user) -> handlerToWidget $ runDB $ do
            snowdrift_member <- isProjectAffiliated "snowdrift" user_id
            projects <- select $ from $ \ project -> return project

            messages :: [Entity Message] <- select $ from $ \ message -> do
                            where_ $ let readSnowdriftMessages =
                                             ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                                               &&. isNothing (message ^. MessageTo))

                                         readUserMessages =
                                            ( message ^. MessageCreatedTs >=. val (userReadMessages user)
                                              &&. message ^. MessageTo ==. val (Just user_id))

                                      in if snowdrift_member
                                          then readSnowdriftMessages ||. readUserMessages
                                          else readUserMessages

                            return message


            applications :: [Entity VolunteerApplication] <- if is_committee_member
                             then select $ from $ \ application -> do
                                where_ ( application ^. VolunteerApplicationCreatedTs >=. val (userReadApplications user) )
                                return application
                            else return []
            {-
            edits :: [Entity WikiEdit] <- select $ from $ \ wiki_edit -> do
                    where_ ( wiki_edit ^. WikiEditTs >=. val (userReadEdits user) ) -- &&. wiki_edit ^. WikiEditUser !=. val user_id )
                    return wiki_edit

            comments :: [Entity Comment] <- select $ from $ \ comment -> do
                    where_ ( comment ^. CommentCreatedTs >=. val (userReadComments user) ) -- &&. comment ^. CommentUser !=. val user_id )
                    return comment
            -}
            let unval (Value a) = a
                foldCounts (c1, c2) (c1', c2') =
                    (c1 + unval (L.head c1'), c2 + unval (L.head c2'))

            counts <- getCounts (Entity user_id user) projects

            let (comments, edits) = foldl foldCounts (0, 0) counts

            return (messages, applications, edits, comments)

    $(widgetFile "navbar")

