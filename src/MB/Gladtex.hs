module MB.Gladtex
    ( gladTex
    , checkForGladtex
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
import MB.Types

gladtexProgName :: String
gladtexProgName = "gladtex"

gladTex :: Config -> FilePath -> String -> IO ()
gladTex config htexPath color = do
  let args = [ "-d"
             , imageDir config
             , "-u"
             , baseUrl config ++ "/generated-images/"
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
