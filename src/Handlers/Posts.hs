module Handlers.Posts
    (
    ) where

import Types (Post (..))

mkPostSplice :: Post -> Splice AppHandler
mkPostSplice p = runChildrenWithText
    [ ("posttitle", title p)
    , ("postbody", body p)
    , ("time", showAsText $ time p)
    ]
