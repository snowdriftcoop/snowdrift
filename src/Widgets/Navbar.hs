
module Widgets.Navbar where

import Import

navbar :: Widget
navbar = do
    maybe_user <- handlerToWidget maybeAuth
    alreadyExpired

    num_unread_notifs <- return (0 :: Int)
    $(widgetFile "navbar")
