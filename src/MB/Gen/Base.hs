module MB.Gen.Base
    ( buildPage
    )
where

import System.IO
import System.Exit

import qualified MB.Files as Files
import MB.Types
import MB.Templates

buildPage :: Handle -> Blog -> String -> Maybe String -> IO ()
buildPage h blog content extraTitle = do
  eTmpl <- loadTemplate $ Files.pageTemplatePath blog

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          let attrs = [ ("content", content)
                      ] ++ maybe [] (\t -> [("extraTitle", t)]) extraTitle

          writeTemplate blog h tmpl attrs
          hClose h
