module View.ResetPassphrase where

import Import hiding (ResetPassword)

import DeprecatedBootstrap

data ResetPassword = ResetPassword
    { rpHandle :: Maybe Text
    , rpEmail  :: Maybe Text
    }

resetPasswordForm :: Form ResetPassword
resetPasswordForm = renderBootstrap3 BootstrapBasicForm $ ResetPassword
    <$> aopt' textField  "Account Name" Nothing
    <*> aopt' emailField "Email" Nothing
