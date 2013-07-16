{-# LANGUAGE OverloadedStrings #-}

module Handlers.Admin
    ( handleAdminPosts
    , handleAdminPostAdd
    , handleAdminPostEdit
    , handleAdminPostDelete
    , handleAdminGuestbook
    , handleAdminGuestbookEdit
    , handleAdminGuestbookDelete
    ) where

import Control.Applicative
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Heist.Interpreted
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Text.Digestive
import Text.Digestive.Heist
import Text.Digestive.Snap

import Application
import Db
import Utils

data PostKind = News
    deriving (Eq)

data FormPost = FormPost
    { fptitle   :: Text
    , fpbody    :: Text
    , fpkind    :: PostKind
    }

instance Show PostKind where
    show News = "wiesc"

kinds :: [(PostKind, Text)]
kinds = [ (News, "Wieść")
        ]

postAddForm :: Monad m => Form Text m FormPost
postAddForm = FormPost
    <$> "title" .: check' "Brak tytułu"
    <*> "body"  .: check' "Brak treści"
    <*> "kind"  .: choice kinds Nothing

renderPost :: DbPost -> Splice (Handler App App)
renderPost p = runChildrenWithText
    [ ("id", showAsText $ fromMaybe 0 $ pId p)
    , ("title", pTitle p)
    , ("kind", pKind p)
    , ("body", T.take 50 $ pBody p)
    , ("creation", showAsText $ pCreation p)
    , ("publish", showAsText $ pPublish p)
    ]

renderGuestbook :: DbGuestbook -> Splice (Handler App App)
renderGuestbook g = runChildrenWith
    [ ("id", textSplice . showAsText $ fromMaybe 0 $ gId g)
    , ("nick", textSplice $ gNick g)
    , ("email", textSplice $ gEmail g)
    , ("www", textSplice $ gWww g)
    , ("body", textSplice $ T.take 50 $ gBody g)
    , ("creation", textSplice . showAsText $ gCreation g)
    ]

handleAdminPosts :: Handler App App ()
handleAdminPosts = do
    posts <- with db getAllPosts
    heistLocal (splices posts) $ render "/admin-posts"
  where
    splices ps = bindSplices [("posts", mapSplices renderPost ps)]

handleAdminPostAdd :: Handler App App ()
handleAdminPostAdd = do
    (view, mresult) <- runForm "postadd" postAddForm
    case mresult of
        Just post -> do
            ltime <- liftIO getCurrentTime
            let dbpost = DbPost Nothing
                                (fptitle post)
                                (fpbody post)
                                (T.toLower . T.pack $ show $ fpkind post)
                                ltime
                                ltime
            with db $ savePost dbpost
            redirect "/admin/wpisy"
        Nothing   -> bindDS view
  where
    bindDS v = heistLocal (bindDigestiveSplices v) $ render "admin-post-add"

handleAdminPostEdit :: Handler App App ()
handleAdminPostEdit = undefined

handleAdminPostDelete :: Handler App App ()
handleAdminPostDelete = do
    mid <- getParam "postid"
    case mid of
        Just pid -> do
            with db $ deletePost $ bs2i pid
            redirect "/admin/wpisy"
        Nothing -> writeBS "error"

handleAdminGuestbook :: Handler App App ()
handleAdminGuestbook = do
    guestbook <- with db getGuestbook
    heistLocal (splices guestbook) $ render "/admin-guestbook"
  where
    splices gs = bindSplices [("guestbook", mapSplices renderGuestbook gs)]

handleAdminGuestbookDelete :: Handler App App ()
handleAdminGuestbookDelete = do
    mid <- getParam "gid"
    case mid of
        Just gid -> do
            with db $ deleteGuestbook $ bs2i gid
            redirect "/admin/ksiega"
        Nothing -> writeBS "error"

handleAdminGuestbookEdit :: Handler App App ()
handleAdminGuestbookEdit = undefined
