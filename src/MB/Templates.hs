module MB.Templates
    ( renderTemplate
    , loadTemplate
    )
where

import Text.StringTemplate
    ( newSTMP
    , render
    , setManyAttrib
    , checkTemplate
    )
import MB.Types
    ( Template
    )

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