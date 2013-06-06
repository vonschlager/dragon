{-# LANGUAGE OverloadedStrings #-}

module Handlers.Albums
    ( handleAlbums
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Attoparsec (parse, IResult(..))
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as S
import Network.Http.Client

import Heist.Interpreted
import Snap.Core (writeBS)
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Utils

data Album = Album
    { albumid :: Text
    , title   :: Text
    , thumb   :: [Thumb]
    }

newtype Albums = Albums [Album]

data Thumb = Thumb
    { url    :: Text
    , width  :: Integer
    , height :: Integer
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
                                 <*> o .: "height"
                                 <*> o .: "width"
    parseJSON _          = mzero

picasaApiUrl = "http://picasaweb.google.com/data/feed/api/user/"

picasaUser = "115396442595599374875"

picasaApiVer = "2"

picasaMethod = "json"

picasaFields = "entry(media:group(media:title,media:thumbnail),gphoto:id)"

jsonHandler :: FromJSON a => Response -> InputStream B.ByteString -> IO (Maybe a)
jsonHandler p i = (decode . BL.fromChunks) <$> S.toList i

getAlbums :: Handler App App Albums
getAlbums = do
    malbums <- liftIO $ get (picasaApiUrl <> picasaUser
                <> "?alt=" <> picasaMethod
                <> "&v=" <> picasaApiVer
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
