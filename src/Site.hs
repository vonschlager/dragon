{-# LANGUAGE OverloadedStrings #-}

module Site
    ( app
    ) where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Heist
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.Heist.Interpreted
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.SqliteSimple
import Snap.Util.FileServe

import Application
import Handlers.Admin
import Handlers.Albums
import Handlers.Auth
import Handlers.Guestbook
import Handlers.News
import Handlers.Photos
import Splices

routes :: [(ByteString, Handler App App ())]
routes = [ ("/admin/logowanie", with auth handleLogin)
         , ("/admin/wyloguj", with auth handleLogout)
         , ("/admin", redirect "/admin/wpisy")
         , ("/admin/wpisy", handleAdminPosts)
         , ("/admin/wpis/dodaj", handleAdminPostAdd)
         , ("/admin/wpis/edytuj/:postid", handleAdminPostEdit)
         , ("/admin/wpis/usun/:postid", handleAdminPostDelete)
         , ("/admin/ksiega", handleAdminGuestbook)
         , ("/admin/ksiega/usun/:gid", handleAdminGuestbookDelete)
         , ("/wpis/pokaz/:postid", handlePostView)
         , ("/wiesci", handleNews)
         , ("/wiesci/strona/:page", handleNewsRange)
         , ("/ksiega", handleGuestbook)
         , ("/galeria", handleAlbums)
         , ("/zdjecia/:albumid", handlePhotos)
         , ("/", redirect "/wiesci")
         , ("/static", serveDirectory "static")
         ]

splices :: [(Text, Splice (Handler App App))]
splices = [ ("posts", postsSplice)
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
    return $ App h s a d
  where
    desc = "Strona domowa 7 DH Dragon"
