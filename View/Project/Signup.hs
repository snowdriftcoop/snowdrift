module View.Project.Signup where

import Import hiding
    ( ProjectSignupName, ProjectSignupWebsite, ProjectSignupHandle
    , ProjectSignupStartDate, ProjectSignupLocation, ProjectSignupApplicantRole
    , ProjectSignupMission, ProjectSignupGoals, ProjectSignupFundsUse
    , ProjectSignupAdditionalInfo, ProjectSignupLegalStatus )

import Model.License.Internal
import Model.Project.Signup
import Model.Project.Signup.Internal

import           Control.Applicative (liftA2)
import qualified Data.Text           as Text
import           Data.String         (fromString)
import           Data.Hourglass      (timeGetDate, dateYear, Month (..))
import           System.Hourglass    (timeCurrent)
import           Text.Blaze.Internal (preEscapedText)

projectSignupForm :: [License] -> Form ProjectSignup
projectSignupForm ls = renderBootstrap3 BootstrapBasicForm $ ProjectSignup
    <$> reqc ProjectSignupName      textField "Project name"
    <*> optc ProjectSignupWebsite   urlField  "Website"
    <*> reqc ProjectSignupHandle    textField "Desired handle on the site"
    <*> reqc ProjectSignupStartDate dateField "Project start date"
    <*> (((?:) project_licenses)
              <$> optn (multiSelectFieldList $ licenses ls) project_licenses
              <*> optc OtherProjectSignupLicense textField
                      "If other, please describe")
    <*> reqn (multiSelectFieldList categories)
            "Primary project categories (multiple can be selected)"
    <*> optc ProjectSignupCategoryComment textField
            "Optional comments about project categories"
    <*> optc ProjectSignupLocation textField
            "Location project is legally based out of"
    <*> (((?<|>) project_legal_status)
              <$> optn (selectFieldList legalStatuses) project_legal_status
              <*> optc OtherProjectSignupLegalStatus textField
                      "If other, please describe")
    <*> reqc ProjectSignupApplicantRole  textField
            "What role do you have with this project?"
    <*> reqc ProjectSignupMission        textareaField
            "What is your project's mission?"
    <*> reqc ProjectSignupGoals          textareaField
            "What are the project's goals?"
    <*> reqc ProjectSignupFundsUse       textareaField
            (fromString $ "How will the project benefit from and " <>
             "make use of funds raised through Snowdrift.coop?")
    <*> optc ProjectSignupAdditionalInfo textareaField
            (fromString $ "Please provide any additional information, like " <>
             "contacts of others affiliated with the project")
  where
    project_licenses     = "Project licenses"
    project_legal_status = "Project legal status"

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

-- XXX: Maybe use custom fields for these, so errors could be shown as
-- alerts.

(?:) :: Text -> Maybe [a] -> Maybe a -> [a]
(?:) t Nothing   Nothing  = error $ "at least one " <> show t <> " value is required"
(?:) _ (Just xs) (Just x) = x:xs
(?:) _ (Just xs) Nothing  = xs
(?:) _ Nothing   (Just x) = [x]

(?<|>) :: Text
       -> Maybe ProjectSignupLegalStatus
       -> Maybe ProjectSignupLegalStatus
       -> ProjectSignupLegalStatus
(?<|>) t Nothing  Nothing  =
    error $ show t <> " value is required"
(?<|>) t (Just x) (Just y) =
    error $
        "one " <> show t <> " value is required, but got two: " <>
        Text.unpack (ppProjectLegalStatus x) <> ", " <>
        Text.unpack (ppProjectLegalStatus y)
(?<|>) _ (Just x) Nothing  = x
(?<|>) _ Nothing  (Just y) = y

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
ppProjectLegalStatus (ProjectSignupLegalStatus      Unincorporated) = "unincorporated"
ppProjectLegalStatus (ProjectSignupLegalStatus      BenefitCorp)    = "benefit corp"
ppProjectLegalStatus (ProjectSignupLegalStatus      NonProfitCoop)  = "non-profit coop"
ppProjectLegalStatus (ProjectSignupLegalStatus      ForProfitCoop)  = "for-profit coop"
ppProjectLegalStatus (OtherProjectSignupLegalStatus s)              = s

ppMap :: (Enum c, Bounded c) => (c -> t) -> (t -> p) -> [(p, t)]
ppMap c f =
    flip map [minBound .. maxBound] $ \x ->
        let y = c x in (f y, y)

licenses :: [License] -> [(Text, ProjectSignupLicense)]
licenses ls =
    flip map ls $ \l ->
        (unLicenseName $ licenseName l, ProjectSignupLicense l)

categories :: [(Text, ProjectSignupCategory)]
categories = ppMap id ppProjectCategory

legalStatuses :: [(Text, ProjectSignupLegalStatus)]
legalStatuses = ppMap ProjectSignupLegalStatus ppProjectLegalStatus
