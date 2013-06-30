{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( showAsText
    , bs2integer
    , bs2text
    , text2bs
    , if'
    , check'
    , prettyMonth
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Char8 as BS
import Text.Digestive

showAsText :: Show a => a -> Text
showAsText = T.pack . show

bs2integer :: ByteString -> Integer
bs2integer = read . BS.unpack

bs2text :: ByteString -> Text
bs2text = T.decodeUtf8

text2bs :: Text -> ByteString
text2bs = T.encodeUtf8

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

check' :: Monad m => Text -> Form Text m Text
check' err = check err (not . T.null) $ text Nothing

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
