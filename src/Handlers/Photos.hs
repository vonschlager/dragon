{-# LANGUAGE OverloadedStrings #-}

module Handlers.Photos
    ( handlePhotos
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
import Snap.Core (writeBS, getParam)
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application

data Photo = Photo
    { purl  :: Text
    , thumb :: [Thumb]
    }

newtype Photos = Photos [Photo]

data Thumb = Thumb
    { turl   :: Text
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
picasaFields = "entry(media:group(media:thumbnail),content)"

jsonHandler :: FromJSON a => Response -> InputStream B.ByteString -> IO (Maybe a)
jsonHandler _ i = (decode . BL.fromChunks) <$> S.toList i

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
    case maid of
        Just aid -> do
            (Photos photos) <- getPhotos aid
            heistLocal (splices photos) $ render "/photos"
        Nothing  -> writeBS "Error"
  where
    splices ps = bindSplices [("photos", mapSplices renderPhoto ps)] 
