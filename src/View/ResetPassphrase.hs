module View.ResetPassphrase where

import Import hiding (ResetPassphrase)

import DeprecatedBootstrap

data ResetPassphrase = ResetPassphrase
    { rpHandle :: Maybe Text
    , rpEmail  :: Maybe Text
    }

resetPassphraseForm :: Form ResetPassphrase
resetPassphraseForm = renderBootstrap3 BootstrapBasicForm $ ResetPassphrase
    <$> aopt' textField  "Account Name" Nothing
    <*> aopt' emailField "Email"  Nothing
