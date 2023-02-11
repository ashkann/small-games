{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod.Core

data Links = Links

mkYesod "Links" [parseRoutes|
    / HomeR GET
    /page1 Page1R GET
    /page2 Page2R GET
|]

instance Yesod Links where
    makeSessionBackend _ = return Nothing

getHomeR = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1 !|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2 !|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go to home !|]

main :: IO ()
main = warp 3000 Links