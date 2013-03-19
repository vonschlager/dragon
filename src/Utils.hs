{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( showAsText
    , bs2text
    , mkSlug
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

showAsText :: Show a => a -> Text
showAsText = T.pack . show

bs2text :: ByteString -> Text
bs2text = T.decodeUtf8

mkSlug :: Text -> Text
mkSlug = T.replace " " "-" . T.toLower . T.strip
