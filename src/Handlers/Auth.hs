{-# LANGUAGE OverloadedStrings #-}

module Handlers.Auth
  ( handleLogin
  , handleLogout
  ) where

import           Control.Applicative
import           Data.Text (Text)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Digestive.Snap

import           Application
import           Utils (text2bs, nonEmptyText)

data User = User
    { login    :: Text
    , password :: Text
    }

loginForm :: Monad m => Form Text m User
loginForm = User
    <$> "login"    .: nonEmptyText
    <*> "password" .: nonEmptyText

handleLogin :: Handler App (AuthManager App) ()
handleLogin = do
    (view, mresult) <- runForm "login" loginForm 
    case mresult of
        Just user -> do 
            elogin <- login' user    
            case elogin of
                Left  _ -> renderLogin view
                Right _ -> redirect "/"
          where
            login' u = loginByUsername (text2bs $ login u)
                                       (ClearText $ text2bs $ password u)
                                       False
        Nothing   -> renderLogin view
  where
    renderLogin v = heistLocal (bindDigestiveSplices v) $ render "login" 

handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"
