{-# LANGUAGE OverloadedStrings #-}

module Db
    ( DbPost(..)
    , getPostKind
    , getPost
    , getAllPosts
    , getNewsByYearMonth
    , getNewsLastYearMonth
    , getMatch
    , savePost
    , deletePost
    , DbGuestbook(..)
    , getGuestbook
    , getGBookByYearMonth
    , getGBookLastYearMonth
    , deleteGuestbook
    , DbYMSideNav(..)
    , getNewsSideNav
    , getGBookSideNav
    , DbMatchSideNav(..)
    , getMatchSideNav
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
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
    { gId       :: Maybe Integer
    , gNick     :: Text
    , gEmail    :: Text
    , gWww      :: Text
    , gBody     :: Text
    , gCreation :: UTCTime
    }

instance FromRow DbGuestbook where
    fromRow = DbGuestbook <$> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field
                          <*> field

data DbYMSideNav = DbYMSideNav
    { nsnYear   :: Text
    , nsnMonths :: [Text]
    }

instance FromRow DbYMSideNav where
    fromRow = DbYMSideNav <$> field <*> (T.split (==',') <$> field)

data DbMatchSideNav = DbMatchSideNav
    { msnTitle :: Text
    }

instance FromRow DbMatchSideNav where
    fromRow = DbMatchSideNav <$> field

savePost :: DbPost -> Handler App Sqlite ()
savePost p =
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

getGBookByYearMonth :: Text -> Text -> Handler App Sqlite [DbGuestbook]
getGBookByYearMonth y m =
    flip query [y <> " " <> m] $
        "SELECT id,nick,email,www,body,creation "
        <> "FROM guestbook "
        <> "WHERE STRFTIME('%Y %m', creation) LIKE ? "
        <> "ORDER BY creation DESC"

getGBookLastYearMonth :: Handler App Sqlite (Text, Text)
getGBookLastYearMonth =
    liftM head $ query_ $
        "SELECT STRFTIME('%Y', creation),STRFTIME('%m', creation) "
        <> "FROM (SELECT MAX(creation) AS creation "
        <> "FROM guestbook)"

getNewsLastYearMonth :: Handler App Sqlite (Text, Text)
getNewsLastYearMonth =
    liftM head $ query_ $
        "SELECT STRFTIME('%Y', publish),STRFTIME('%m', publish) "
        <> "FROM (SELECT MAX(publish) AS publish "
        <> "FROM posts WHERE kind = 'wiesc')"

getNewsByYearMonth :: Text -> Text -> Handler App Sqlite [DbPost]
getNewsByYearMonth y m =
    flip query [y <> " " <> m] $
        "SELECT id,title,body,kind,creation,publish "
        <> "FROM posts WHERE kind = 'wiesc' "
        <> "AND STRFTIME('%Y %m', publish) LIKE ? "
        <> "ORDER BY publish DESC"

getMatch :: Handler App Sqlite [DbPost]
getMatch =
    query_ $ "SELECT id,title,body,kind,creation,publish "
        <> "FROM posts WHERE kind = 'zapalka' "
        <> "ORDER BY publish DESC"

deleteGuestbook :: Integer -> Handler App Sqlite ()
deleteGuestbook i =
    execute "DELETE FROM guestbook WHERE id=?" [i]

getNewsSideNav :: Handler App Sqlite [DbYMSideNav]
getNewsSideNav =
    query_ $ "SELECT STRFTIME('%Y', publish) AS year,"
        <> "GROUP_CONCAT(DISTINCT(STRFTIME('%m', publish))) "
        <> "FROM posts GROUP BY year ORDER BY publish DESC"

getGBookSideNav :: Handler App Sqlite [DbYMSideNav]
getGBookSideNav =
    query_ $ "SELECT STRFTIME('%Y', creation) AS year,"
        <> "GROUP_CONCAT(DISTINCT(STRFTIME('%m', creation))) "
        <> "FROM guestbook GROUP BY year ORDER BY creation DESC"

getMatchSideNav :: Handler App Sqlite [DbMatchSideNav]
getMatchSideNav = 
    query_ $ "SELECT STRFTIME('%Y', publish) AS year,"
        <> "GROUP_CONCAT(DISTINCT(STRFTIME('%m', publish))) "
        <> "FROM posts GROUP BY year ORDER BY publish DESC"
