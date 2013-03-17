module Utils
    ( showAsText
    ) where

import Data.Text

showAsText :: Show a => a -> Text
showAsText = pack . show
