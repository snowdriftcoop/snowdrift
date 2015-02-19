module View.Project.Signup where

import Import hiding
    ( ProjectSignupName, ProjectSignupWebsite, ProjectSignupHandle
    , ProjectSignupStartDate, ProjectSignupLocation, ProjectSignupApplicantRole
    , ProjectSignupMission, ProjectSignupGoals, ProjectSignupFundsUse
    , ProjectSignupAdditionalInfo, ProjectSignupLegalStatus
    , ProjectSignupLegalStatusComment )

import Model.License.Internal
import Model.Project.Signup
import Model.Project.Signup.Internal
import Widgets.Markdown

import           Control.Applicative (liftA2)
import qualified Data.Text           as Text
import           Data.String         (fromString)
import           Data.Hourglass      (timeGetDate, dateYear, Month (..))
import           System.Hourglass    (timeCurrent)
import           Text.Blaze.Internal (preEscapedText)

projectSignupForm :: (Route App -> Text) -> [License] -> Form ProjectSignup
projectSignupForm render ls = renderBootstrap3 BootstrapBasicForm $ ProjectSignup
    <$> reqc ProjectSignupName      textField "Project name"
    <*> optc ProjectSignupWebsite   textField "Website"
    <*> reqc ProjectSignupHandle    textField
            (fromString $ "Desired project handle (will be shown at " <>
             handle <> ")")
    <*> reqc ProjectSignupStartDate dateField "Project start date"
    <*> reqn (multiSelectFieldList $ licenses ls)
            "Project licenses (multiple can be selected)"
    <*> optc ProjectSignupLicenseComment textField
            "If other license, please describe"
    <*> reqn (multiSelectFieldList categories)
            "Primary project categories (multiple can be selected)"
    <*> optc ProjectSignupCategoryComment textField
            "Optional comments about project categories"
    <*> optc ProjectSignupLocation textField
            "Location project is legally based out of"
    <*> reqn (selectFieldList legalStatuses) "Project legal status"
    <*> optc ProjectSignupLegalStatusComment textField
            "Optional details about legal status"
    <*> reqn (selectFieldList coopStatuses) "Is your project a co-op?"
    <*> reqc ProjectSignupApplicantRole  textField
            "What role do you have with this project?"
    <*> reqc ProjectSignupMission        snowdriftMarkdownField
            "What is your project's mission?"
    <*> reqc ProjectSignupGoals          snowdriftMarkdownField
            "What are the project's goals in the near- and medium-term?"
    <*> reqc ProjectSignupFundsUse       snowdriftMarkdownField
            (fromString $ "How will the project benefit from and " <>
             "make use of funds raised through Snowdrift.coop?")
    <*> optc ProjectSignupAdditionalInfo snowdriftMarkdownField
            (fromString $ "Any additional comments, questions, or " <>
             "information? Consider providing contact info for others " <>
             "affiliated with the project")
  where
    handle = Text.unpack $ render $ ProjectR "handle"

dateField :: Field Handler (Year, Month)
dateField = Field
    { fieldParse   = \rawVals _ ->
          case rawVals of
              [year, month] -> pair
                  (fieldParse yearsField [year] [])
                  (fieldParse monthsField [month] [])
              [] -> return $ Right Nothing
              _  -> return $ Left "You must enter two values"
    , fieldView    = \idAttr nameAttr otherAttrs eRes isReq -> do
          asWidget "<div class=\"form-inline\">"
          fieldView yearsField (idAttr <> "-year")
              nameAttr otherAttrs (fst <$> eRes) isReq
          fieldView monthsField (idAttr <> "-month")
              nameAttr otherAttrs (snd <$> eRes) isReq
          asWidget "</div>"
    , fieldEnctype = UrlEncoded
    }
  where
    asWidget    = toWidget . preEscapedText . Text.pack
    pair        = (liftA2 . liftA2 . liftA2) (,)
    yearsField  = selectField years
    monthsField = selectFieldList months

reqn :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
     => Field m a -> SomeMessage (HandlerSite m) -> AForm m a
reqn f s = areq' f s Nothing

optn :: MonadHandler m
     => Field m a -> SomeMessage (HandlerSite m) -> AForm m (Maybe a)
optn f s = aopt' f s Nothing

reqc :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
     => (a -> b) -> Field m a -> SomeMessage (HandlerSite m) -> AForm m b
reqc c f s = c <$> reqn f s

optc :: MonadHandler m
     => (a -> b) -> Field m a -> SomeMessage (HandlerSite m)
     -> AForm m (Maybe b)
optc c f s = (c <$>) <$> (optn f s)

years :: Handler (OptionList Year)
years = do
    current_year <- liftIO $ dateYear . timeGetDate <$> timeCurrent
    optionsPairs $ map (\x -> (Text.pack $ show x, Year x))
        [current_year, pred current_year .. 1980]

months :: [(Text, Month)]
months = map (\x -> (Text.pack $ show x, x)) $ enumFrom January

ppProjectCategory :: ProjectSignupCategory -> Text
ppProjectCategory CreativeWriting = "creative writing"
ppProjectCategory Education       = "education"
ppProjectCategory Games           = "games"
ppProjectCategory HardwareDesign  = "hardware design"
ppProjectCategory Journalism      = "journalism"
ppProjectCategory Music           = "music"
ppProjectCategory OnlineService   = "online service"
ppProjectCategory Research        = "research"
ppProjectCategory Software        = "software"
ppProjectCategory Video           = "video"
ppProjectCategory VisualArt       = "visual art"

ppProjectLegalStatus :: ProjectSignupLegalStatus -> Text
ppProjectLegalStatus NonProfitTaxDeductible =
    "public-benefit with tax-deductible donations, as in 501(c)(3) in US"
ppProjectLegalStatus NonProfitNotTaxDeductible =
    "other public-benefit status, such as 501(c)(4) and/or state-level designation"
ppProjectLegalStatus TradeOrganization =
    "trade organization serving business interests, as in 501(c)(6)"
ppProjectLegalStatus ForProfitSocial =
    "benefit corp or similar for-profit with added social mission"
ppProjectLegalStatus ForProfitTraditional = "traditional for-profit"
ppProjectLegalStatus Unincorporated = "unincorporated"

ppProjectCoopStatus :: ProjectSignupCoopStatus -> Text
ppProjectCoopStatus Coop    = "yes"
ppProjectCoopStatus NotCoop = "no"

ppProjectSignupLicense :: ProjectSignupLicense -> Text
ppProjectSignupLicense (ProjectSignupLicense l)  = unLicenseName $ licenseName l
ppProjectSignupLicense OtherProjectSignupLicense = "other"

licenses :: [License] -> [(Text, ProjectSignupLicense)]
licenses ls = flip map ls' $ \l -> (ppProjectSignupLicense l, l)
  where
    ls' = map ProjectSignupLicense ls <> [OtherProjectSignupLicense]

mapBounds :: (Enum b, Bounded b) => (b -> a) -> [(a, b)]
mapBounds f = flip map [minBound .. maxBound] $ \x -> (f x, x)

categories :: [(Text, ProjectSignupCategory)]
categories = mapBounds ppProjectCategory

legalStatuses :: [(Text, ProjectSignupLegalStatus)]
legalStatuses = mapBounds ppProjectLegalStatus

coopStatuses :: [(Text, ProjectSignupCoopStatus)]
coopStatuses = mapBounds ppProjectCoopStatus
