{-# LANGUAGE CPP, RecordWildCards #-}

module Handler.Image where

#if MIN_VERSION_base(4,8,0)
import Import hiding (foldMap)
#else
import Import
#endif

import Yesod.Core.Types
import Data.Conduit
import Data.Conduit.List (foldMap)
import Control.Monad.Trans.Resource
import Data.Text.Encoding

import Widgets.Time

getImageR :: Text -> Handler TypedContent
getImageR image_handle = do
    Entity _ Image{..} <- runYDB $ getBy404 $ UniqueImageHandle image_handle
    respond imageFormat imageData

getImageMetaR :: Text -> Handler Html
getImageMetaR image_handle = do
    Entity _ Image{..} <- runYDB $ getBy404 $ UniqueImageHandle image_handle
    defaultLayout $(widgetFile "image_metadata")

uploadForm :: Form (Text, FileInfo)
uploadForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField "Image Name" Nothing
    <*> fileAFormReq "Image File"

getUploadImageR :: Handler Html
getUploadImageR = do
    _ <- requireAuthId

    (form, enctype) <- generateFormPost uploadForm

    defaultLayout $(widgetFile "upload_image")

postUploadImageR :: Handler Html
postUploadImageR = do
    user_id <- requireAuthId

    ((result, _), _) <- runFormPost uploadForm

    now <- liftIO getCurrentTime

    case result of
        FormMissing  -> error "form missing"
        FormFailure err  -> error $ "error processing form:\n" ++ unlines (map (('\t':) . show) err)
        FormSuccess (name, FileInfo{..}) -> do
            contents <- liftIO $ runResourceT $ fileSourceRaw $$ foldMap id
            maybe_image_id <- runDB $ insertUnique $ Image now user_id Nothing name Nothing (encodeUtf8 fileContentType) contents

            case maybe_image_id of
                Just _ -> redirect $ ImageMetaR name
                Nothing -> do
                    setMessage "that name is already taken, try another"
                    unnamed_image_id <- runYDB $ insert $ UnnamedImage now user_id Nothing (Just name) Nothing (encodeUtf8 fileContentType) contents
                    redirect $ NameImageR unnamed_image_id


nameImageForm :: Maybe Text -> Form Text
nameImageForm = renderBootstrap3 BootstrapBasicForm . areq textField "New Image Name"

nameImage :: Text -> UnnamedImage -> Image
nameImage name (UnnamedImage ts uploader project _ origin format contents) = Image ts uploader project name origin format contents

getNameImageR :: UnnamedImageId -> Handler Html
getNameImageR unnamed_image_id = do
    viewer_id <- requireAuthId
    UnnamedImage{..} <- runYDB $ get404 unnamed_image_id

    when (viewer_id /= unnamedImageUploader) $ permissionDenied "you did not upload this image"

    (form, enctype) <- generateFormPost $ nameImageForm unnamedImageName

    defaultLayout $(widgetFile "name_image")

postNameImageR :: UnnamedImageId -> Handler Html
postNameImageR unnamed_image_id = do
    viewer_id <- requireAuthId
    unnamed_image@UnnamedImage{..} <- runYDB $ get404 unnamed_image_id

    when (viewer_id /= unnamedImageUploader) $ permissionDenied "you did not upload this image"

    ((result, _), _) <- runFormPost $ nameImageForm unnamedImageName

    case result of
        FormMissing  -> error "form missing"
        FormFailure err  -> error $ "error processing form:\n" ++ unlines (map (('\t':) . show) err)
        FormSuccess name -> do
            maybe_image_id <- runDB $ insertUnique $ nameImage name unnamed_image
            case maybe_image_id of
                Just _ -> redirect $ ImageMetaR name
                Nothing -> do
                    setMessage "that name is also already taken, try another"

                    runYDB $ update $ \ ui -> do
                        where_ $ ui ^. UnnamedImageId ==. val unnamed_image_id
                        set ui [ UnnamedImageName =. val (Just name) ]

                    redirect $ NameImageR unnamed_image_id
