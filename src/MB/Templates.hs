module MB.Templates
    ( renderTemplate
    , fillTemplate
    , writeTemplate
    , withTemplate
    )
where

import Control.Applicative
import Control.Monad.Trans
import Data.Maybe
import System.IO
import System.Exit
import Text.StringTemplate
    ( newSTMP
    , render
    , setManyAttrib
    , checkTemplate
    )
import MB.Types

withTemplate :: FilePath -> (Template -> BlogM a) -> BlogM a
withTemplate path f = do
  tmpl <- loadTemplate path
  case tmpl of
    Left msg -> do
              liftIO $ putStrLn $ "Could not load template " ++ show path ++ ": " ++ msg
              liftIO $ exitFailure
    Right t -> f t

loadTemplate :: FilePath -> BlogM (Either String Template)
loadTemplate path = do
  s <- liftIO $ readFile path
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

writeTemplate :: Blog -> Handle -> Template -> [(String, String)] -> BlogM ()
writeTemplate blog h t attrs = liftIO $ hPutStr h $ fillTemplate blog t attrs

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
