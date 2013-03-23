{-# LANGUAGE OverloadedStrings #-}

module Db
    ( Post (..)
    , getPosts
    , getPost
    , savePost
    ) where

import Control.Applicative
import Control.Monad (liftM)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Snap.Snaplet
import Snap.Snaplet.SqliteSimple

import Application
import Utils (showAsText, if')

data Post = Post
    { id :: Maybe Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , time :: UTCTime
    }

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field <*> field <*> field

savePost :: Post -> Handler App Sqlite ()
savePost p = do
    rows <- query "SELECT id FROM posts WHERE slug = ?" [slug p] :: Handler App Sqlite [Only Integer]
    let count = length rows 
    let lslug = if' (count > 0) (T.concat [slug p, T.pack "-", (showAsText $ count + 1)]) $ slug p
    execute "INSERT INTO posts (title,body,slug,time) VALUES(?,?,?,?)"
        (title p, body p, lslug, time p)

getPosts :: Handler App Sqlite [Post]
getPosts =
    query_ "SELECT id,title,body,slug,time FROM posts ORDER BY time DESC"

getPost :: Text -> Handler App Sqlite Post
getPost s =
    liftM head $ query "SELECT id,title,body,slug,time FROM posts WHERE slug = ? LIMIT 1" [s]
