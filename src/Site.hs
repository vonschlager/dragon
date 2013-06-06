{-# LANGUAGE OverloadedStrings #-}

module Site
    ( app
    ) where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.Heist.Interpreted
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.SqliteSimple
import Snap.Util.FileServe
import Heist
import Heist.Interpreted

import Application
import Handlers.Auth
import Handlers.Posts
import Handlers.Navbar
import Handlers.Albums
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
         , ("/albums", handleAlbums)
         , ("/", redirect "/post/kind/news")
         , ("/static", serveDirectory "static")
         ]

splices :: [(Text, Splice (Handler App App))]
splices = [ ("navbar", navbarSplice)
          , ("kinds", kindsSplice)
          , ("posts", postsSplice)
          ]

app :: SnapletInit App App
app = makeSnaplet "app" desc Nothing $ do
    addRoutes routes
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
            initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    d <- nestSnaplet "db" db sqliteInit
    a <- nestSnaplet "auth" auth $ initSqliteAuth sess d
    addConfig h $ mempty { hcInterpretedSplices = splices }
    --addAuthSplices auth
    return $ App h s a d
  where
    desc = "Strona domowa 7 DH Dragon"
