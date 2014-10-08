module Handler.License where

import Import
import Model.License
--import Data.Maybe
import Data.ByteString as S
import Data.ByteString.Lazy as L
--import Control.Monad.Trans.Resource
--import Control.Monad.ST
import Data.Conduit
import Data.Conduit.Binary

licenseEntryForm :: Html -> MForm Handler (FormResult LicenseFormData, Widget)
licenseEntryForm lefHtml = do
    (licenseNameRes, licenseNameView) <- mreq textField (generateFieldSettings "LicenseName" [("class", "form-control"), ("placeholder", "License Name")]) Nothing
    (licenseClassificationRes, licenseClassificationView) <- mreq (selectFieldList getLicenseClassificationTypes) (generateFieldSettings "Classification" [("class", "form-control"), ("placeholder", "License Classification")]) Nothing
    (licenseProjectTypeRes, licenseProjectTypeView) <- mreq (selectFieldList getLicenseProjectTypes) (generateFieldSettings "ProjectType" [("class", "form-control"), ("placeholder", "Project Type")]) Nothing
    (licenseOptionsRes, licenseOptionsView) <- mopt textareaField (generateFieldSettings "Options" [("class", "form-control"), ("placeholder", "License Options")]) Nothing
    (licenseWebsiteRes, licenseWebsiteView) <- mreq textField (generateFieldSettings "Website" [("class", "form-control"), ("placeholder", "Web Link to Legal Text")]) Nothing
    (licenseIconRes, licenseIconView) <- mopt fileField (generateFieldSettings "UploadIcon" [("class", "form-control")]) Nothing

    let licenseEntryRes = LicenseFormData
                        <$> licenseNameRes
                        <*> licenseClassificationRes
                        <*> licenseProjectTypeRes
                        <*> licenseOptionsRes
                        <*> licenseWebsiteRes
                        <*> licenseIconRes
    let widget = toWidget $(widgetFile "license_entry")
    return (licenseEntryRes, widget)


getLicenseEntryR :: Handler Html
getLicenseEntryR = do
    ((_, widget), enctype) <- runFormGet licenseEntryForm
    defaultLayout $ do
        setTitle "License Entry | Snowdrift.coop"
        [whamlet|
            <H1 Align="center">
                Snowdrift.coop - Admin Site
            <H3 Align="center">
                License Entry Form
            <form .form-horizontal method=POST action=@{LicenseEntryR} enctype=#{enctype}>
                ^{widget}
                <input type=submit>
        |]

extractImage :: Maybe FileInfo -> Handler(Maybe S.ByteString)
extractImage Nothing  = (return Nothing) :: Handler(Maybe S.ByteString)
extractImage (Just x) = fmap (Just . toStrict) ((fileSource x $$ sinkLbs) :: Handler L.ByteString)


postLicenseEntryR :: Handler Html
postLicenseEntryR = do
    ((result, widget), enctype) <- runFormPostNoToken $ licenseEntryForm
    case result of
        FormSuccess l -> do
            limage <- extractImage $ lfdImage l
            let license = License {
                              licenseName = lfdName l
                            , licenseClassification = lfdClassification l
                            , licenseProjectType = lfdProjectType l
                            , licenseOptions = lfdOptions l
                            , licenseWebsite = lfdWebsite l
                            , licenseImage = limage
                            }

            runDB $ insert_ license
            defaultLayout $ do
                setTitle "Successful License Entry | Snowdrift.coop" 
                [whamlet|<H1 Align="Center">License Entered
                         <br>
                         <p>#{licenseName license} has been added to the database.
                         <form method=GET action=@{LicenseEntryR} enctype=#{enctype}>
                            <button>Enter another License
                |]
        
        FormFailure messages -> defaultLayout $ do
            setTitle "License Entry Failure | Snowdrift.coop"
            [whamlet|
                 <H1 Align="Center">License Error(s):
                 <br>
                 <ul>
                    $forall message <- messages
                         <li>#{message}
                 <br>
                <form .form-horizontal method=POST action=@{LicenseEntryR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>
            |]
        
        FormMissing -> defaultLayout $ do
            setTitle "License Entry | Snowdrift.coop"
            [whamlet|
                <H1 Align="center">
                    Snowdrift.coop - Admin Site
                <H3 Align="center">
                    License Entry Form
                <form .form-horizontal method=POST action=@{LicenseEntryR} enctype=#{enctype}>
                    ^{widget}
                    <input type=submit>
            |]
