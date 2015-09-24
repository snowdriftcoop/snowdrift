module View.ResetPassword where

import Import hiding (ResetPassword)

data ResetPassword = ResetPassword
    { rpHandle :: Maybe Text
    , rpEmail  :: Maybe Text
    }

resetPasswordForm :: Form ResetPassword
resetPasswordForm = renderBootstrap3 BootstrapBasicForm $ ResetPassword
    <$> aopt' textField  "Handle" Nothing
    <*> aopt' emailField "Email"  Nothing
