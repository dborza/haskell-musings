{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod
import Text.Hamlet (shamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Char (toLower)
import Data.List (sort)

data Links = Links

data Person = Person
    { name :: String
    , age  :: Int
    }

mkYesod "Links" [parseRoutes|
/index HomeR GET
/page1 Page1R GET
/page2 Page2R GET
/hamlet HamletR GET
|]

instance Yesod Links

getHomeR  = defaultLayout [whamlet|<a href=@{Page1R}>Go to page 1!|]
getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Go to page 2!|]
getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]
getHamletR = defaultLayout [whamlet|
        $forall person <- people
             <p>Hello, my name is #{name person} and I am #{age person}.
             <p>
                 Let's do some funny stuff with my name: #
                 <b>#{sort $ map toLower (name person)}
             <p>Oh, and in 5 years I'll be #{show ((+) 5 (age person))} years old.
             |]
               where
                 people = [
                        Person "Michael" 26,
                        Person "Gogu" 25,
                        Person "casdas" 34,
                        Person "asdsad" 44,
                        Person "asdfdsfds" 3,
                        Person "asadsf" 4,
                        Person "dsfsdf" 5,
                        Person "dfsdf" 5,
                        Person "sdfsdfsdf" 45

                    ]

main = warp 3000 Links