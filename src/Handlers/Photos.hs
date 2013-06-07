{-# LANGUAGE OverloadedStrings #-}

module Handlers.Photos
    ( handlePhotos
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
import Snap.Core (writeBS, getParam)
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Utils

data Photo = Photo
    { purl  :: Text
    , thumb :: [Thumb]
    }

newtype Photos = Photos [Photo]

data Thumb = Thumb
    { turl   :: Text
    , width  :: Integer
    , height :: Integer
    }

instance FromJSON Photo where
    parseJSON (Object o) =
         Photo <$> ((o .: "content")     >>= (.: "src"))
               <*> ((o .: "media$group") >>= (.: "media$thumbnail"))
    parseJSON _          = mzero

instance FromJSON Photos where
    parseJSON (Object o) = Photos <$> ((o .: "feed") >>= (.: "entry"))
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

picasaFields = "entry(media:group(media:thumbnail),content)"

jsonHandler :: FromJSON a => Response -> InputStream B.ByteString -> IO (Maybe a)
jsonHandler p i = (decode . BL.fromChunks) <$> S.toList i

getPhotos :: B.ByteString -> Handler App App Photos
getPhotos aid = do
    mphotos <- liftIO $ get (picasaApiUrl <> picasaUser
                <> "/albumid/" <> aid
                <> "?alt=" <> picasaMethod
                <> "&v=" <> picasaApiVer
                <> "&thumbsize=180u&imgmax=800u"
                <> "&fields=" <> picasaFields) jsonHandler
    case mphotos of
        Just photos -> return photos
        Nothing     -> return $ Photos []

renderPhoto :: Photo -> Splice (Handler App App)
renderPhoto p = runChildrenWithText
    [ ("url", purl p)
    , ("thumb", turl $ head $ thumb p)
    ]

handlePhotos :: Handler App App ()
handlePhotos = do
    maid <- getParam "albumid"
    let aid = case maid of
                Just aid -> aid
                Nothing  -> "0"
    (Photos photos) <- getPhotos aid
    heistLocal (splices photos) $ render "/photos"
  where
    splices ps = bindSplices [("photos", mapSplices renderPhoto ps)] 
