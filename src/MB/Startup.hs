module MB.Startup
    ( findBaseDir
    , baseDirEnvName
    , baseDirParamName
    , usage
    , argValue
    )
where

import Control.Monad
    ( mplus
    )
import Data.List
    ( isPrefixOf
    )
import Data.Maybe
    ( listToMaybe
    )

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
