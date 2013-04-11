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
import Handlers.Navbar
import Splices

routes :: [(ByteString, Handler App App ())]
routes = [ ("/login", with auth handleLoginSubmit)
         , ("/logout", with auth handleLogout)
         , ("/newuser", with auth handleNewUser)
         , ("/post/add", handlePostAdd)
         , ("/post/view/:postid", handlePostView)
         , ("/post/edit/:postid", handlePostEdit)
         , ("/post/delete/:postid", handlePostDelete)
         , ("/post/kind/:kind", handlePostKind)
         , ("/navbar", handleNavbar)
         , ("/navbar/add", handleNavbarAdd)
         , ("/navbar/add/kind", handleNavbarAddKind)
         , ("/navbar/add/post", handleNavbarAddPost)
         , ("/navbar/add/other", handleNavbarAddOther)
         , ("/navbar/edit/:entryid", handleNavbarEdit)
         , ("/navbar/delete/:entryid", handleNavbarDelete)
         , ("/", redirect "/post/kind/news")
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
    addSplices [ ("navbar", navbarSplice)
               , ("kinds", kindsSplice)
               , ("posts", postsSplice)
               ]
    addAuthSplices auth
    return $ App h s a d
  where
    desc = "Strona domowa 7 DH Dragon"

