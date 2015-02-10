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

projectSignupForm :: [License] -> Form ProjectSignup
projectSignupForm ls = renderBootstrap3 BootstrapBasicForm $ ProjectSignup
    <$> reqc ProjectSignupName      textField "Project name"
    <*> optc ProjectSignupWebsite   textField "Website"
    <*> reqc ProjectSignupHandle    textField "Desired handle on the site"
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
            (fromString $ "Please provide any additional information, like " <>
             "contacts of others affiliated with the project")

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
ppProjectCategory Research        = "research"
ppProjectCategory Software        = "software"
ppProjectCategory Video           = "video"
ppProjectCategory VisualArt       = "visual art"

ppProjectLegalStatus :: ProjectSignupLegalStatus -> Text
ppProjectLegalStatus Unincorporated = "unincorporated"
ppProjectLegalStatus BenefitCorp    = "benefit corp"
ppProjectLegalStatus NonProfitCoop  = "non-profit coop"
ppProjectLegalStatus ForProfitCoop  = "for-profit coop"
ppProjectLegalStatus OtherNonProfit = "other non-profit"
ppProjectLegalStatus OtherForProfit = "other for-profit"

ppProjectSignupLicense :: ProjectSignupLicense -> Text
ppProjectSignupLicense (ProjectSignupLicense l)  = unLicenseName $ licenseName l
ppProjectSignupLicense OtherProjectSignupLicense = "other"

licenses :: [License] -> [(Text, ProjectSignupLicense)]
licenses ls = flip map ls' $ \l -> (ppProjectSignupLicense l, l)
  where
    ls' = map ProjectSignupLicense ls <> [OtherProjectSignupLicense]

categories :: [(Text, ProjectSignupCategory)]
categories =
    flip map [minBound .. maxBound] $ \x -> (ppProjectCategory x, x)

legalStatuses :: [(Text, ProjectSignupLegalStatus)]
legalStatuses =
    flip map [minBound .. maxBound] $ \x -> (ppProjectLegalStatus x, x)
