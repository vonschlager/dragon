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
import Data.Text (Text)
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application

data DbPost = DbPost
    { postid :: Maybe Integer
    , title :: Text
    , body :: Text
    , kind :: Text
    , time :: UTCTime
    }

instance FromRow DbPost where
    fromRow = DbPost <$> field <*> field <*> field <*> field <*> field

savePost :: DbPost -> Handler App Sqlite ()
savePost p = do
    execute "INSERT INTO posts (title,body,kind,time) VALUES(?,?,?,?)"
        (title p, body p, kind p, time p)

deletePost :: Integer -> Handler App Sqlite ()
deletePost i =
    execute "DELETE FROM posts WHERE id = ?" [i]

getPostKind :: Text -> Handler App Sqlite [DbPost]
getPostKind k =
    query "SELECT id,title,body,kind,time FROM posts WHERE kind = ?" [k]

getPost :: Integer -> Handler App Sqlite DbPost
getPost i =
    liftM head $ query "SELECT id,title,body,kind,time FROM posts WHERE id = ?" [i]

getAllPosts :: Handler App Sqlite [DbPost]
getAllPosts =
    query_ "SELECT id,title,body,kind,time FROM posts"
