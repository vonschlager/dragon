{-# LANGUAGE OverloadedStrings #-}

module Handlers.Auth
  ( handleLogin
  , handleLogout
  ) where

import Control.Applicative
import Data.Text (Text)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Heist
import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap

import Application
import Utils

data User = User
    { login    :: Text
    , password :: Text
    }

loginForm :: Monad m => Form Text m User
loginForm = User
    <$> "login"    .: check' "Brak loginu"
    <*> "password" .: check' "Brak hasła"

handleLogin :: Handler App (AuthManager App) ()
handleLogin = do
    (view, mresult) <- runForm "login" loginForm 
    case mresult of
        Just user -> do 
            elogin <- login' user    
            case elogin of
                Left  _ -> bindDS view $ render "login-invalid"
                Right _ -> redirect "/admin"
          where
            login' u = loginByUsername (login u)
                                       (ClearText $ text2bs $ password u)
                                       False
        Nothing   -> bindDS view $ render "login"
  where
    bindDS v = heistLocal (bindDigestiveSplices v)

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
