module MB.Gen.Base
    ( buildPage
    )
where

import MB.Types
import MB.Templates

buildPage :: Blog -> String -> Maybe String -> Template -> String
buildPage blog content extraTitle tmpl =
    let attrs = [ ("content", content)
                ] ++ maybe [] (\t -> [("extraTitle", t)]) extraTitle
    in fillTemplate blog tmpl attrs
