{-# LANGUAGE OverloadedStrings #-}

module Db
    ( DbPost(..)
    , getPostKind
    , getPost
    , getAllPosts
    , savePost
    , deletePost
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
    { postid   :: Maybe Integer
    , title    :: Text
    , body     :: Text
    , kind     :: Text
    , creation :: UTCTime
    , publish  :: UTCTime
    }

instance FromRow DbPost where
    fromRow = DbPost <$> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field
                     <*> field

savePost :: DbPost -> Handler App Sqlite ()
savePost p = do
    flip execute (title p, body p, kind p, creation p, publish p) $
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
