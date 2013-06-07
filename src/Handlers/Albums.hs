{-# LANGUAGE OverloadedStrings #-}

module Handlers.Albums
    ( handleAlbums
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.IO.Streams (InputStream)
import qualified System.IO.Streams as S
import Network.Http.Client

import Heist.Interpreted
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application

data Album = Album
    { albumid :: Text
    , title   :: Text
    , thumb   :: [Thumb]
    }

newtype Albums = Albums [Album]

data Thumb = Thumb
    { url    :: Text
    }

instance FromJSON Album where
    parseJSON (Object o) =
         Album <$> ((o .: "gphoto$id")   >>= (.: "$t"))
               <*> ((o .: "media$group") >>= (.: "media$title") >>= (.: "$t"))
               <*> ((o .: "media$group") >>= (.: "media$thumbnail"))
    parseJSON _          = mzero

instance FromJSON Albums where
    parseJSON (Object o) = Albums <$> ((o .: "feed") >>= (.: "entry"))
    parseJSON _          = mzero

instance FromJSON Thumb where
    parseJSON (Object o) = Thumb <$> o .: "url"
    parseJSON _          = mzero

picasaApiUrl :: B.ByteString
picasaApiUrl = "http://picasaweb.google.com/data/feed/api/user/"

picasaUser :: B.ByteString
picasaUser = "115396442595599374875"

picasaApiVer :: B.ByteString
picasaApiVer = "2"

picasaMethod :: B.ByteString
picasaMethod = "json"

picasaFields :: B.ByteString
picasaFields = "entry(media:group(media:title,media:thumbnail),gphoto:id)"

jsonHandler :: FromJSON a => Response -> InputStream B.ByteString -> IO (Maybe a)
jsonHandler _ i = (decode . BL.fromChunks) <$> S.toList i

getAlbums :: Handler App App Albums
getAlbums = do
    malbums <- liftIO $ get (picasaApiUrl <> picasaUser
                <> "?alt=" <> picasaMethod
                <> "&v=" <> picasaApiVer
                <> "&thumbsize=130c"
                <> "&fields=" <> picasaFields) jsonHandler
    case malbums of
        Just albums -> return albums
        Nothing     -> return $ Albums []

renderAlbum :: Album -> Splice (Handler App App)
renderAlbum a = runChildrenWithText
    [ ("albumid", albumid a)
    , ("title", title a)
    , ("thumb", url $ head $ thumb a)
    ]

handleAlbums :: Handler App App ()
handleAlbums = do
    (Albums albums) <- getAlbums
    heistLocal (splices albums) $ render "/albums"
  where
    splices as = bindSplices [("albums", mapSplices renderAlbum as)] 
