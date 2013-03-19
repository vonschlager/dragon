{-# LANGUAGE OverloadedStrings #-}

module Site
    ( app
    ) where

import Data.ByteString (ByteString)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.Heist
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.SqliteSimple
import Snap.Util.FileServe

import Application
import Handlers.Auth
import Handlers.Posts

routes :: [(ByteString, Handler App App ())]
routes = [ ("/login", with auth handleLoginSubmit)
         , ("/logout", with auth handleLogout)
         , ("/newuser", with auth handleNewUser)
         , ("/post/add", handlePostAdd)
         , ("/post/view/:slug", handlePostView)
         , ("/post/edit/:slug", handlePostEdit)
         , ("/post/delete/:slug", handlePostDelete)
         , ("/posts", handlePosts)
         , ("/", redirect "/posts")
         , ("", serveDirectory "static")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" desc Nothing $ do
    addRoutes routes
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
            initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addAuthSplices auth
    return $ App h s a d
  where
    desc = "Strona domowa 7 DH Dragon"

