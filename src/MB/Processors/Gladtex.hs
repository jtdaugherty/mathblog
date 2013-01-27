module MB.Processors.Gladtex
    ( gladtexProcessor
    )
where

import Control.Applicative ((<$>))
import System.Exit
    ( ExitCode(..)
    , exitFailure
    )
import System.Directory
    ( copyFile
    )
import System.Process
    ( readProcessWithExitCode
    )
import Control.Monad
    ( when
    )
import qualified Text.Pandoc as Pandoc
import MB.Types

gladtexProcessor :: Processor
gladtexProcessor =
    nullProcessor { applyWriterOptions = Just gladtexWriterOptions
                  , postProcessPost = Just processGladtex
                  , buildPostTitle = Just gladtexTitle
                  , checkDataDir = Just $ const checkForGladtex
                  }

gladtexWriterOptions :: Pandoc.WriterOptions -> Pandoc.WriterOptions
gladtexWriterOptions opts =
    opts { Pandoc.writerHTMLMathMethod = Pandoc.GladTeX
         }

gladtexProgName :: String
gladtexProgName = "gladtex"

processGladtex :: Blog -> FilePath -> Page -> IO ()
processGladtex blog path page = do
  -- Copy the final HTML path (which was passed in) to temp file since
  -- we ultimately need to write to the final HTML path.
  let htexPath = path ++ ".htex"
      textColor = case page of
                    Index -> "0000FF"
                    BlogPost -> "000000"

  copyFile path htexPath

  -- gladtex will overwrite the original .html file.
  gladTex blog htexPath textColor

gladTex :: Blog -> FilePath -> String -> IO ()
gladTex blog htexPath color = do
  let args = [ "-d", imageDir blog
             , "-u", "/generated-images/"
             , "-r", "120"
             , "-s", "4"
             , "-b", "FFFFFF"
             , "-c", color
             , "-l", "displaymath"
             , "-i", "math"
             , htexPath
             ]

  (ecode, _, err) <- readProcessWithExitCode gladtexProgName args ""

  when (ecode /= ExitSuccess) $ do
    putStrLn $ "Error processing " ++ (show htexPath) ++ " with " ++ gladtexProgName ++ ":"
    putStrLn err
    exitFailure

checkForGladtex :: IO ()
checkForGladtex = do
  (code, _, _) <- readProcessWithExitCode gladtexProgName [] ""
  case code of
    (ExitFailure c) ->
        do
          putStrLn $ "This program requires '" ++ gladtexProgName ++
                       "'; I attempted to run it but " ++
                       "got exit status " ++ (show c)
          exitFailure
    _ -> return ()

gladtexTitle :: Page -> [Pandoc.Inline] -> [Pandoc.Inline]
gladtexTitle BlogPost ts = rewriteInline 190 <$> ts
gladtexTitle Index ts = rewriteInline 110 <$> ts

rewriteInline :: Int -> Pandoc.Inline -> Pandoc.Inline
rewriteInline dpi (Pandoc.Math v s) =
    Pandoc.Math v $ "<EQ ENV=\"math\" DPI=\"" ++ show dpi ++ "\">" ++ s ++ "</EQ>"
rewriteInline _ i = i
