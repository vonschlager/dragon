{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( showAsText
    , bs2i
    , bs2t
    , t2bs
    , if'
    , check'
    , mkElement
    , prettyMonth
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 as BS
import Text.Digestive
import Text.XmlHtml (Node)
import qualified Text.XmlHtml as X

showAsText :: Show a => a -> Text
showAsText = T.pack . show

bs2i :: ByteString -> Integer
bs2i = read . BS.unpack

bs2t :: ByteString -> Text
bs2t = T.decodeUtf8

t2bs :: Text -> ByteString
t2bs = T.encodeUtf8

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

check' :: Monad m => Text -> Form Text m Text
check' err = check err (not . T.null) $ text Nothing

mkElement :: Text -> [(Text, Text)] -> [Node] -> [Node]
mkElement elem attrs ns = [X.Element elem attrs ns]

prettyMonth :: Text -> Text
prettyMonth m | m == "01" = "Styczeń"
              | m == "02" = "Luty"
              | m == "03" = "Marzec"
              | m == "04" = "Kwiecień"
              | m == "05" = "Maj"
              | m == "06" = "Czerwiec"
              | m == "07" = "Lipiec"
              | m == "08" = "Sierpień"
              | m == "09" = "Wrześień"
              | m == "10" = "Październik"
              | m == "11" = "Listopad"
              | m == "12" = "Grudzień"
              | otherwise = m
