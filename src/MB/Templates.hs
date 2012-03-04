module MB.Templates
    ( renderTemplate
    , loadTemplate
    , fillTemplate
    , writeTemplate
    )
where

import Control.Applicative
import Data.Maybe
import System.IO
import Text.StringTemplate
    ( newSTMP
    , render
    , setManyAttrib
    , checkTemplate
    )
import MB.Types

loadTemplate :: FilePath -> IO (Either String Template)
loadTemplate path = do
  s <- readFile path
  s `seq` return ()

  let (a, _, _) = checkTemplate t
      t = newSTMP s

  case a of
    Nothing -> return $ Right t
    Just msg -> return $ Left $ "Error parsing template " ++ path ++ ": " ++ msg

renderTemplate :: [(String, String)] -> Template -> String
renderTemplate attrs = render . setManyAttrib attrs

fillTemplate :: Blog -> Template -> [(String, String)] -> String
fillTemplate blog t attrs = renderTemplate attrs' t
    where attrs' = commonTemplateAttrs blog ++ attrs

writeTemplate :: Blog -> Handle -> Template -> [(String, String)] -> IO ()
writeTemplate blog h t attrs = hPutStr h $ fillTemplate blog t attrs

commonTemplateAttrs :: Blog -> [(String, String)]
commonTemplateAttrs blog =
    [ ( "baseUrl", baseUrl blog )
    , ( "title", title blog )
    , ( "authorName", authorName blog )
    , ( "authorEmail", authorEmail blog )
    , ( "extraPageHead", extraPageHead blog )
    ]

extraPageHead :: Blog -> String
extraPageHead b = concat $ catMaybes $ pageHead <$> processors b
