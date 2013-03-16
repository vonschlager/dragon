module Utils
    ( showAsText
    ) where

import qualified Data.Text as T

showAsText :: Show a => a -> Text
showAsText = T.pack . show
