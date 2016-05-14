{-# LANGUAGE ScopedTypeVariables #-}
module AuthSpec (spec) where

import TestImport

-- import Network.Wai.Test (SResponse(..))

spec :: Spec
spec = withApp $ do
    describe "password reset" $ do
        it "can't reuse tokens" $ do
            runDB $ do
                uid <- insert (User "foo@bap" True Nothing)
                void $ insert (VerifyEmail "twiddledee" uid)

            get ("/auth/page/email/verify/1/twiddledee" ::Text)
            statusIs 200
            l :: [Entity VerifyEmail] <- runDB (selectList [] [])
            pure $ l `shouldSatisfy` null
