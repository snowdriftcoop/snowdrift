{-# LANGUAGE OverloadedStrings #-}
module UserTest
    ( userSpecs
    ) where

import Prelude
import TestImport

import Control.Monad

userSpecs :: Spec
userSpecs = do
    let users :: [NamedUser]
        users = [minBound .. maxBound]

    ydescribe "user" $ do
        yit "creates a user" $ [marked|
            forM_ users $ \user -> do
                get200 CreateAccountR

                withStatus 303 True $ request $ do
                    setUrl CreateAccountR
                    addToken
                    setMethod "POST"
                    byLabel "Handle (private):" (username user)
                    byLabel "Passphrase:" (password user)
                    byLabel "Repeat passphrase:" (password user)
        |]

        yit "logs in as a user" $ [marked|
            forM_ users $ \user -> do
                loginAs user
        |]
