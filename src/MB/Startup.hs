{-# LANGUAGE CPP #-}
module MB.Startup
    ( StartupConfig(..)
    , startupConfigFromEnv
    , versionString
#ifdef TESTING
    , Flag(..)
    , startupConfig
    , baseDirEnvName
    , baseDirParamName
    , htmlOutputDirEnvName
    , htmlDirParamName
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
import System.FilePath
import System.Console.GetOpt

data StartupConfig = StartupConfig { listenMode :: Bool
                                   , dataDirectory :: FilePath
                                   , initDataDirectory :: Bool
                                   , forceRegeneration :: Bool
                                   , htmlOutputDirectory :: FilePath
                                   , configFilePath :: FilePath
                                   }
                     deriving (Show, Eq)

data Flag = Listen
          | DataDir FilePath
          | InitDataDirectory
          | ForceRegenerate
          | HtmlOutputDir FilePath
          | ConfigFile FilePath
          | Version
          | Help
            deriving (Eq)

getDataDirFlag :: [Flag] -> Maybe FilePath
getDataDirFlag [] = Nothing
getDataDirFlag (DataDir p:_) = Just p
getDataDirFlag (_:fs) = getDataDirFlag fs

getHtmlOutputDirFlag :: [Flag] -> Maybe FilePath
getHtmlOutputDirFlag [] = Nothing
getHtmlOutputDirFlag (HtmlOutputDir p:_) = Just p
getHtmlOutputDirFlag (_:fs) = getHtmlOutputDirFlag fs

getConfigFilePath :: [Flag] -> Maybe FilePath
getConfigFilePath [] = Nothing
getConfigFilePath (ConfigFile p:_) = Just p
getConfigFilePath (_:fs) = getConfigFilePath fs

options :: [OptDescr Flag]
options = [ Option ['d'] [baseDirParamName] (ReqArg DataDir "PATH")
                       $ "Path where blog input data files (configuration, posts,\n" ++
                         "etc.) are located.  If this is not specified,\n" ++
                         baseDirEnvName ++ " must be set in the environment."

          , Option ['i'] ["init"] (NoArg InitDataDirectory)
                       $ "Initialize the blog data directory."
          , Option ['l'] ["listen"] (NoArg Listen)
                       $ "Make mb poll periodically and regenerate your\n" ++
                             "blog content when something changes.  This is\n" ++
                             "useful if you want to run a local web server\n" ++
                             "to view your posts as you're writing them."
          , Option ['f'] ["force"] (NoArg ForceRegenerate)
                       "Force a rebuild of all blog content."
          , Option ['o'] [htmlDirParamName] (ReqArg HtmlOutputDir "PATH")
                       $ "The directory where HTML and image output should be\n" ++
                       "written.  If this is not specified, " ++ htmlOutputDirEnvName ++
                       "\nmust be set in the environment."
          , Option ['c'] ["config-file"] (ReqArg ConfigFile "FILENAME")
                       $ "Use the specified config file instead of 'blog.cfg'\n" ++
                         "in the data directory.  This path must be relative\n" ++
                         "to the data directory."
          , Option ['v'] ["version"] (NoArg Version)
                       "The version of this mathblog installation."
          , Option ['h'] ["help"] (NoArg Help)
                       "This help output."
          ]

versionString :: String
versionString = "mathblog version 0.5"

-- |Inspect the program environment to create a startup configuration.
-- If the configuration information is invalid or absent, this will
-- automatically emit usage info and exit the program.
startupConfigFromEnv :: IO StartupConfig
startupConfigFromEnv = do
  env  <- getEnvironment
  args <- getArgs

  let (flags, _, _) = getOpt Permute options args

  when (Version `elem` flags) $
           (putStrLn versionString >> exitFailure)

  when (Help `elem` flags) $
           (usage >> exitFailure)

  case startupConfig flags env of
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
startupConfig :: [Flag] -> [(String, String)] -> Maybe StartupConfig
startupConfig flags env = do
  let lm = Listen `elem` flags
      i = InitDataDirectory `elem` flags
      f = ForceRegenerate `elem` flags

  d <- getDataDirFlag flags `mplus` (lookup baseDirEnvName env)
  o <- getHtmlOutputDirFlag flags `mplus` (lookup htmlOutputDirEnvName env)
  c <- getConfigFilePath flags `mplus` (Just $ d </> "blog.cfg")

  return $ StartupConfig { listenMode = lm
                         , dataDirectory = d
                         , initDataDirectory = i
                         , forceRegeneration = f
                         , htmlOutputDirectory = o
                         , configFilePath = c
                         }

baseDirEnvName :: String
baseDirEnvName = "MB_DATA_DIR"

baseDirParamName :: String
baseDirParamName = "data-dir"

htmlOutputDirEnvName :: String
htmlOutputDirEnvName = "MB_OUTPUT_DIR"

htmlDirParamName :: String
htmlDirParamName = "output-dir"

usage :: IO ()
usage = do
  let h = concat [ "Usage: mb [args] (or set " ++ baseDirEnvName ++
                   " and " ++ htmlOutputDirEnvName ++ " in the environment)\n\n"
                 , "mb is a tool for creating and managing a mathematically-inclined\n"
                 , "weblog.\n"
                 ]
  putStrLn $ usageInfo h options
