{-# LANGUAGE OverloadedStrings #-}

module Site
    ( app
    ) where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Snap.Core
import Snap.Snaplet
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
import Handlers.Photos
import Handlers.Admin
import Splices

routes :: [(ByteString, Handler App App ())]
routes = [ ("/admin", handleAdminPosts) 
         , ("/admin/login", with auth handleLogin)
         , ("/admin/logout", with auth handleLogout)
         , ("/admin/navbar", handleNavbar)
         , ("/admin/navbar/add", handleNavbarAdd)
         , ("/admin/navbar/add/kind", handleNavbarAddKind)
         , ("/admin/navbar/add/post", handleNavbarAddPost)
         , ("/admin/navbar/add/other", handleNavbarAddOther)
         , ("/admin/navbar/edit/:entryid", handleNavbarEdit)
         , ("/admin/navbar/delete/:entryid", handleNavbarDelete)
         , ("/admin/post/add", handleAdminPostAdd)
         , ("/admin/post/edit/:postid", handleAdminPostEdit)
         , ("/admin/post/delete/:postid", handleAdminPostDelete)
         , ("/post/view/:postid", handlePostView)
         , ("/post/kind/:kind", handlePostKind)
         , ("/albums", handleAlbums)
         , ("/photos/:albumid", handlePhotos)
         , ("/", redirect "/post/kind/news")
         , ("/static", serveDirectory "static")
         ]

splices :: [(Text, Splice (Handler App App))]
splices = [ ("navbar", navbarSplice)
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
