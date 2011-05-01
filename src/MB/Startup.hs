{-# LANGUAGE CPP #-}
module MB.Startup
    ( StartupConfig(..)
    , startupConfigFromEnv
#ifdef TESTING
    , startupConfig
    , baseDirEnvName
    , baseDirParamName
    , getDataDirFlag
#endif
    )
where

import Control.Monad
    ( mplus
    , when
    )
import System.Exit
    ( exitFailure
    )
import System.Environment
    ( getEnvironment
    , getArgs
    )
import System.Console.GetOpt

data StartupConfig = StartupConfig { listenMode :: Bool
                                   , dataDirectory :: FilePath
                                   }

data Flag = Listen | DataDir FilePath
            deriving (Eq)

getDataDirFlag :: [Flag] -> Maybe FilePath
getDataDirFlag [] = Nothing
getDataDirFlag (DataDir p:_) = Just p
getDataDirFlag (_:fs) = getDataDirFlag fs

options :: [OptDescr Flag]
options = [ Option ['d'] [baseDirParamName] (ReqArg DataDir "PATH")
                       $ "Path where blog files will be stored.  If this is\n" ++
                             "not specified, " ++ baseDirEnvName ++ " must be set in the\n" ++
                             "environment."

          , Option ['l'] ["listen"] (NoArg Listen)
                       $ "Make mb poll periodically and regenerate your\n" ++
                             "blog content when something changes.  This is\n" ++
                             "useful if you want to run a local web server\n" ++
                             "to view your posts as you're writing them."
          ]

-- |Inspect the program environment to create a startup configuration.
-- If the configuration information is invalid or absent, this will
-- automatically emit usage info and exit the program.
startupConfigFromEnv :: IO StartupConfig
startupConfigFromEnv = do
  env  <- getEnvironment
  args <- getArgs

  case startupConfig args env of
    Nothing -> usage >> exitFailure
    Just conf -> do
      when (head (dataDirectory conf) /= '/') $
           do
             putStrLn $ baseDirEnvName ++ " or " ++
                          "--" ++ baseDirParamName ++ "=<path>" ++
                                   " must specify an absolute path"
             exitFailure

      return conf

-- |Create a startup configuration from the specified arguments list
-- and environment variable list.  Returns Nothing if the required
-- information was absent.
startupConfig :: [String] -> [(String, String)] -> Maybe StartupConfig
startupConfig args env =
    case getOpt Permute options args of
      (_, _, (_:_)) -> Nothing
      (flags, _, []) -> do
        let lm = Listen `elem` flags
        d <- getDataDirFlag flags `mplus` (lookup baseDirEnvName env)

        return $ StartupConfig { listenMode = lm
                               , dataDirectory = d
                               }

baseDirEnvName :: String
baseDirEnvName = "MB_BASE_DIR"

baseDirParamName :: String
baseDirParamName = "baseDir"

usage :: IO ()
usage = do
  let h = concat [ "Usage: mb [args]\n\n"
                 , "mb is a tool for creating and managing a mathematically-inclined\n"
                 , "weblog.\n"
                 ]
  putStrLn $ usageInfo h options
