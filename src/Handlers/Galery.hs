{-# LANGUAGE OverloadedStrings #-}

module Handlers.Galery
    ( handleGalery
    ) where

import Control.Monad.Trans
import Data.Aeson
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8
import Network.HTTP
import Network.URI
import Snap.Core (writeBS)
import Snap.Snaplet
import Snap.Snaplet.Heist

import Application
import Utils

picasaUser :: String
picasaUser = "115396442595599374875"

picasaApiVer :: String
picasaApiVer = "2"

picasaMethod :: String
picasaMethod = "json"

picasaFields :: String
picasaFields = "entry(media:group(media:title,media:thumbnail),gphoto:id)"

downloadURL :: String -> IO (Either String String)
downloadURL url = do
    resp <- simpleHTTP request
    case resp of
        Left x  -> return $ Left $ "error " ++ show x
        Right r ->
            case rspCode r of
                (2,_,_) -> return $ Right (rspBody r)
                (3,_,_) ->
                    case findHeader HdrLocation r of
                        Nothing  -> return $ Left $ show r
                        Just url -> downloadURL url
                _       -> return $ Left $ show r
  where request = Request { rqURI     = uri
                          , rqMethod  = GET
                          , rqHeaders = []
                          , rqBody    = ""
                          }
        uri = fromJust $ parseURI url

handleGalery :: Handler App App ()
handleGalery = do
    resp <- liftIO $ downloadURL $ "http://picasaweb.google.com/data/feed/api/user/"
                                   ++ picasaUser
                                   ++ "?alt=" ++ picasaMethod
                                   ++ "&v=" ++ picasaApiVer
                                   ++ "&fields=" ++ picasaFields
    case resp of
        Left x     -> writeBS $ pack $ "error " ++ show x
        Right json -> writeBS $ pack $ show ((decode $ L.pack json) :: Maybe Value)

