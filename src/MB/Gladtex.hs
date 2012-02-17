module MB.Gladtex
    ( gladtexProcessor
    )
where

import System.Exit
    ( ExitCode(..)
    , exitFailure
    )
import System.Process
    ( readProcessWithExitCode
    )
import Control.Monad
    ( when
    )
import qualified Text.Pandoc as Pandoc
import MB.Types
import qualified MB.Files as Files

gladtexProcessor :: Processor
gladtexProcessor =
    nullProcessor { applyWriterOptions = Just gladtexWriterOptions
                  , processPost = Just processGladtex
                  }

gladtexWriterOptions :: Pandoc.WriterOptions -> Pandoc.WriterOptions
gladtexWriterOptions opts =
    opts { Pandoc.writerHTMLMathMethod = Pandoc.GladTeX
         }

gladtexProgName :: String
gladtexProgName = "gladtex"

processGladtex :: Blog -> Post -> IO Post
processGladtex blog p = do
  return p

gladTex :: Blog -> FilePath -> String -> IO ()
gladTex blog htexPath color = do
  let args = [ "-d"
             , imageDir blog
             , "-u"
             , baseUrl blog ++ "/generated-images/"
             , "-r"
             , "120"
             , "-b"
             , "FFFFFF"
             , "-c"
             , color
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