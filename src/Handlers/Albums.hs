{-# LANGUAGE OverloadedStrings #-}

module Handlers.Albums
    ( handleAlbums
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Aeson
import Data.Attoparsec (parse, IResult(..))
import Data.Maybe
import Data.Monoid
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import Network.Http.Client

import Snap.Core (writeBS)
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Utils

data Album = Album
    { albumid :: String
    , title   :: String
    , thumb   :: [Thumb]
    }

newtype Albums = Albums [Album]
    deriving Show

data Thumb = Thumb
    { url    :: String
    , width  :: Integer
    , height :: Integer
    } deriving Show

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

instance Show Album where
    show a = "Album id: " ++ (albumid a) ++
             " title: " ++ (title a) ++
             " thumb: " ++ (show $ thumb a)

picasaApiUrl = "http://picasaweb.google.com/data/feed/api/user/"

picasaUser = "115396442595599374875"

picasaApiVer = "2"

picasaMethod = "json"

picasaFields = "entry(media:group(media:title,media:thumbnail),gphoto:id)"

decodeAlbums :: B.ByteString -> Maybe Albums
decodeAlbums bs =
    case parse json' bs of
        Done _ v -> case fromJSON v of
                        Success a -> Just a
                        _         -> Nothing
        _        -> Nothing

getJSONbs :: IO B.ByteString
getJSONbs = do
    json <- get (picasaApiUrl <> picasaUser
                <> "?alt=" <> picasaMethod
                <> "&v=" <> picasaApiVer
                <> "&fields=" <> picasaFields) concatHandler
    return json

getJSON :: IO Albums
getJSON = do
    json <- getJSONbs
    case decodeAlbums json of
        Just albums -> return albums
        Nothing     -> writeBS "WTF"

renderAlbums :: Albums -> Splice (Handler App App)
renderAlbums as = runChildrenWithText

handleAlbums :: Handler App App ()
handleAlbums = do
    
    

