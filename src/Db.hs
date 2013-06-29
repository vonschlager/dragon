{-# LANGUAGE OverloadedStrings #-}

module Db
    ( DbPost(..)
    , getPostKind
    , getPost
    , getAllPosts
    , getNewsCount
    , getNewsRange
    , savePost
    , deletePost
    , DbGuestbook(..)
    , getGuestbook
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application

data DbPost = DbPost
    { pId       :: Maybe Integer
    , pTitle    :: Text
    , pBody     :: Text
    , pKind     :: Text
    , pCreation :: UTCTime
    , pPublish  :: UTCTime
    }

instance FromRow DbPost where
    fromRow = DbPost <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

data DbGuestbook = DbGuestbook
    { gId     :: Maybe Integer
    , gNick     :: Text
    , gEmail    :: Text
    , gWww      :: Text
    , gBody    :: Text
    , gCreation :: UTCTime
    }

instance FromRow DbGuestbook where
    fromRow = DbGuestbook <$> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field

instance FromRow Integer where
    fromRow = field

savePost :: DbPost -> Handler App Sqlite ()
savePost p = do
    flip execute (pTitle p, pBody p, pKind p, pCreation p, pPublish p) $
        "INSERT INTO posts (title,body,kind,creation,publish) "
        <> "VALUES(?,?,?,?,?)"

deletePost :: Integer -> Handler App Sqlite ()
deletePost i =
    execute "DELETE FROM posts WHERE id=?" [i]

getPostKind :: Text -> Handler App Sqlite [DbPost]
getPostKind k =
    flip query [k] $ "SELECT id,title,body,kind,creation,publish "
        <> "FROM posts WHERE kind = ? ORDER BY publish DESC"

getPost :: Integer -> Handler App Sqlite DbPost
getPost i =
    liftM head $ flip query [i] $
        "SELECT id,title,body,kind,creation,publish "
        <> " FROM posts WHERE id=?"

getAllPosts :: Handler App Sqlite [DbPost]
getAllPosts =
    query_ $ "SELECT id,title,body,kind,creation,publish "
        <> "FROM posts ORDER BY publish DESC"

getGuestbook :: Handler App Sqlite [DbGuestbook]
getGuestbook =
    query_ $ "SELECT id,nick,email,www,body,creation "
        <> "FROM guestbook ORDER BY creation DESC"

getNewsCount :: Handler App Sqlite Integer
getNewsCount =
    liftM head $ query_ $ "SELECT COUNT(*) FROM posts WHERE kind = 'wiesc'"

getNewsRange :: Integer -> Handler App Sqlite [DbPost]
getNewsRange r =
    flip query [(r-1)*5,5] $ "SELECT id,title,body,kind,creation,publish "
        <> "FROM posts WHERE kind = 'wiesc' ORDER BY publish DESC LIMIT ?,?"
