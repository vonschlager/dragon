{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( showAsText
    , bs2integer
    , bs2text
    , if'
    , mklink
    , mkoption
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 as BS
import qualified Text.XmlHtml as X

showAsText :: Show a => a -> Text
showAsText = T.pack . show

bs2integer :: ByteString -> Integer
bs2integer = read . BS.unpack

bs2text :: ByteString -> Text
bs2text = T.decodeUtf8

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

mklink :: Text -> Text -> X.Node
mklink target text = X.Element "li" [] $ [X.Element "a" [("href", target)] [X.TextNode text]]

mkoption :: Text -> X.Node
mkoption kind = X.Element "option" [("value", kind)] [X.TextNode kind]
