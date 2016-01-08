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
        yit "creates a user" $ do
            forM_ users $ \user -> do
                get200 CreateAccountR

                withStatus 303 True $ request $ do
                    setUrl CreateAccountR
                    addToken
                    setMethod "POST"
                    byLabel "Account name (private, used for logging in):"
                            (username user)
                    byLabel "Passphrase:" (passphrase user)
                    byLabel "Repeat passphrase:" (passphrase user)

        yit "logs in as a user" $ do
            forM_ users $ \user -> do
                loginAs user
