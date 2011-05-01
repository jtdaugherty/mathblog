{-# LANGUAGE CPP #-}
module MB.Startup
    ( StartupConfig(..)
    , startupConfigFromEnv
#ifdef TESTING
    , startupConfig
    , baseDirEnvName
    , argValue
    , findBaseDir
#endif
    )
where

import Control.Monad
    ( mplus
    , when
    )
import Data.List
    ( isPrefixOf
    )
import Data.Maybe
    ( listToMaybe
    , isJust
    )
import System.Exit
    ( exitFailure
    )
import System.Environment
    ( getEnvironment
    , getArgs
    )

data StartupConfig = StartupConfig { listenMode :: Bool
                                   , dataDirectory :: FilePath
                                   }

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
startupConfig args env = do
  d <- findBaseDir args env
  let lm = isJust $ argValue "l" args

  return $ StartupConfig { listenMode = lm
                         , dataDirectory = d
                         }

baseDirEnvName :: String
baseDirEnvName = "MB_BASE_DIR"

baseDirParamName :: String
baseDirParamName = "baseDir"

usage :: IO ()
usage = do
  putStrLn "Usage: mb [-l]\n"
  putStrLn "mb is a tool for creating and managing a mathematically-inclined"
  putStrLn "weblog.  To use mb, you must either set an environment variable or"
  putStrLn "specify the corresponding command-line parameters:"
  putStrLn ""
  putStrLn $ "  " ++ baseDirEnvName ++ ": path where blog files will be stored"
  putStrLn $ "  --" ++ baseDirParamName ++ "=<path>"
  putStrLn ""
  putStrLn " -l: make mb poll periodically and regenerate your blog content"
  putStrLn "     when something changes.  This is useful if you want to run a"
  putStrLn "     local web server to view your posts as you're writing them."



-- | Determine the MB_BASE_DIR, either via a command line parameter or
-- through the environment.  The command line parameter takes
-- precedence over the environment variable.
--
-- Returns @Nothing@ if neither command line argument or env variable
-- is specified.
findBaseDir :: [String] -> [(String, String)] -> Maybe FilePath
findBaseDir args env = let mArgBase = argValue baseDirParamName args
                           mEnvBase = lookup baseDirEnvName env
                       in mArgBase `mplus` mEnvBase

-- | Search a list of strings for a given command line argument.
-- The first param is used as the long argument name, eg:
--
-- > argValue "foo" ["-v", "--foo=bar"]
-- Just "bar"
--
-- > argValue "v" ["-v", "--foo=bar"]
-- Nothing
--
argValue :: String -> [String] -> Maybe String
argValue arg aList = listToMaybe $ map dropKey $ filteredArgs
    where filteredArgs = filter (isPrefixOf $ toArgPfx arg) aList
          dropKey str  = drop 1 (dropWhile (/= '=') str)
          toArgPfx str = "--"++str++"="
