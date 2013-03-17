module Db
    ( Post (..)
    ) where

import Data.Text
import Data.Time.Clock

data Post = Post
    { postId :: Maybe Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , time :: UTCTime
    }

--allPosts :: Connection -> IO [Post]


