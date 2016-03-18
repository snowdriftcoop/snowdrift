{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module View.Project
    ( editProjectForm
    , projectContactForm
    , inviteForm
    , Preview (..)
    , projectConfirmPledgeForm
    , viewForm
    ) where


import Import

import Control.Applicative (liftA2)
import qualified Data.Text as T

import Data.Filter
import Data.Order
import DeprecatedBootstrap
import Handler.Utils
import Model.Project
import Model.Shares
import Model.Role
import Widgets.Markdown

data Preview = Preview | NotPreview deriving Eq

editProjectForm :: Maybe (Project, [Text]) -> Form UpdateProject
editProjectForm mProjTags =
    renderBootstrap3 BootstrapBasicForm $ UpdateProject
        <$> areq' textField "Project Name"
            (projectName <$> mProj)
        <*> areq' textField "Blurb"
            (projectBlurb <$> mProj)
        <*> areq' snowdriftMarkdownField "Description"
            (projectDescription <$> mProj)
        <*> (maybe [] (map T.strip . T.splitOn ",") <$>
            aopt' textField "Tags"
                (Just . T.intercalate ", " <$> mTags))
        <*> aopt' textField
            "GitHub Repository (to show GH tickets here at Snowdrift.coop)"
            (projectGithubRepo <$> mProj)
        -- TODO: system to upload project logo as in SD-543
        -- the following <*> pure Nothing line inserts default logo for now.
        <*> pure Nothing
  where
    mProj = fst <$> mProjTags
    mTags = snd <$> mProjTags

projectContactForm :: Form (Markdown, Language)
projectContactForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq' snowdriftMarkdownField "" Nothing
    <*> areq' (selectField makeLanguageOptions) "Language" Nothing

inviteForm :: Form (Text, Role)
inviteForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq' textField "About this invitation:" Nothing
    <*> areq roleField "Type of Invite:" (Just TeamMember)

viewForm :: Form (Filterable -> Bool, Orderable -> [Double])
viewForm = renderBootstrap3 BootstrapBasicForm $ liftA2 (,)
    (fmap (either (const defaultFilter) id . parseFilterExpression
                                           . fromMaybe "")
          (aopt' textField "filter" Nothing))
    (fmap (either (const defaultOrder) id . parseOrderExpression
                                          . fromMaybe "")
          (aopt' textField "sort" Nothing))

projectConfirmPledgeForm :: Maybe Int64 -> Form SharesPurchaseOrder
projectConfirmPledgeForm =
    renderBootstrap3 BootstrapBasicForm . fmap SharesPurchaseOrder
                                        . areq hiddenField ""
