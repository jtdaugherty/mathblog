{-# LANGUAGE CPP, ScopedTypeVariables #-}
module MB.Startup
    ( startupConfigFromEnv
    , canonicalizeStartupConfig
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

import Prelude hiding (catch)
import Control.Exception
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
import System.Directory
import System.FilePath
import System.Console.GetOpt
import Network.Socket (HostName, PortNumber)

import MB.Types

data Flag = Listen (Maybe String)
          | DataDir FilePath
          | InitDataDirectory
          | ForceRegenerate
          | HtmlOutputDir FilePath
          | ConfigFile FilePath
          | BaseUrl String
          | Version
          | Help
            deriving (Eq)

getDataDirFlag :: [Flag] -> Maybe FilePath
getDataDirFlag [] = Nothing
getDataDirFlag (DataDir p:_) = Just p
getDataDirFlag (_:fs) = getDataDirFlag fs

getListenFlag :: [Flag] -> Maybe (Maybe String)
getListenFlag [] = Nothing
getListenFlag (Listen s:_) = Just s
getListenFlag (_:fs) = getListenFlag fs

getBaseUrlFlag :: [Flag] -> Maybe String
getBaseUrlFlag [] = Nothing
getBaseUrlFlag (BaseUrl s:_) = Just s
getBaseUrlFlag (_:fs) = getBaseUrlFlag fs

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
          , Option ['l'] ["listen"] (OptArg Listen "HOST:PORT")
                       $ "Make mb poll periodically and regenerate your\n" ++
                             "blog content when something changes.  This is\n" ++
                             "useful if you want to run a local web server\n" ++
                             "to view your posts as you're writing them.  Spawns\n" ++
                             "an HTTP server on the specified host and port."
          , Option ['b'] ["base-url"] (ReqArg BaseUrl "URL")
                       $ "The base URL to use in URL generation.  Overrides the\n" ++
                             "'baseUrl' configuration setting."
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
versionString = "mathblog version 0.6"

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

parseListenString :: String -> Maybe (HostName, PortNumber)
parseListenString s =
    case break (== ':') s of
      ([], _) -> Nothing
      (_, []) -> Nothing
      (h, ':':p) -> case reads p :: [(Int, String)] of
                  [] -> Nothing
                  ((i, []):_) -> Just (h, toEnum i)
                  _ -> Nothing
      _ -> Nothing

-- |Create a startup configuration from the specified arguments list
-- and environment variable list.  Returns Nothing if the required
-- information was absent.
startupConfig :: [Flag] -> [(String, String)] -> Maybe StartupConfig
startupConfig flags env = do
  let doInit = InitDataDirectory `elem` flags
      force = ForceRegenerate `elem` flags

  listen <- case getListenFlag flags of
              -- If the listen flag is present, capture the value.
              Just f -> return $ Just f
              Nothing -> return Nothing

  d <- getDataDirFlag flags `mplus` (lookup baseDirEnvName env)
  o <- getHtmlOutputDirFlag flags `mplus` (lookup htmlOutputDirEnvName env)
  c <- getConfigFilePath flags `mplus` (Just $ d </> "blog.cfg")
  let b = getBaseUrlFlag flags

  lAddr <- case listen of
             Nothing -> return Nothing
             -- If the listen flag was given, then it must parse
             -- successfully.
             Just Nothing -> return $ Just ("localhost", 8000)
             Just (Just s) ->
                 (return . Just) =<< parseListenString s

  return $ StartupConfig { dataDirectory = d
                         , initDataDirectory = doInit
                         , forceRegeneration = force
                         , htmlOutputDirectory = o
                         , configFilePath = c
                         , listenAddr = lAddr
                         , overrideBaseUrl = b
                         }

canonicalizeStartupConfig :: StartupConfig -> IO StartupConfig
canonicalizeStartupConfig conf = do
  let dataDirName = takeFileName $ dataDirectory conf
      outputName = takeFileName $ htmlOutputDirectory conf

  d' <- (canonicalizePath $ takeDirectory $ dataDirectory conf) `catch`
        \(_::SomeException) -> do
          putStrLn $ "Invalid data directory: " ++ dataDirectory conf
          exitFailure

  o' <- (canonicalizePath $ takeDirectory $ htmlOutputDirectory conf) `catch`
        \(_::SomeException) -> do
          putStrLn $ "Invalid output directory: " ++ htmlOutputDirectory conf
          exitFailure

  return $ conf { dataDirectory = d' </> dataDirName
                , htmlOutputDirectory = o' </> outputName
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
