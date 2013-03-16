module Types
    ( Post (..)
    ) where

data Post = Post
    { postId :: Maybe Integer
    , title :: Text
    , body :: Text
    , slug :: Text
    , time :: UTCTime
    } deriving (Eg, Show, Read)
