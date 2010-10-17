module MB.Templates
    ( renderTemplate
    , loadTemplate
    )
where

import Control.Applicative
    ( (<$>)
    )
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
  t <- newSTMP <$> readFile path
  let (a, _, _) = checkTemplate t
  case a of
    Nothing -> return $ Right t
    Just s -> return $ Left $ "Error parsing template " ++ path ++ ": " ++ s

renderTemplate :: [(String, String)] -> Template -> String
renderTemplate attrs = render . setManyAttrib attrs