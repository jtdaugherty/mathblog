module MB.Initialize
    ( initializeDataDir
    )
where

import System.Directory
import System.FilePath

import Paths_mathblog
    ( getDataFileName
    )
import MB.Util (copyTree)

defaultConfigFilename :: String
defaultConfigFilename = "blog.cfg"

skelDir :: IO FilePath
skelDir = getDataFileName "skel"

initializeDataDir :: FilePath -> IO ()
initializeDataDir dir = do
  existsDir <- doesDirectoryExist dir

  case existsDir of
    False -> do
      dataDir <- skelDir
      putStrLn $ "Setting up data directory using skeleton: " ++ dataDir
      createDirectory dir
      copyTree dataDir dir

    True -> do
      existsConfig <- doesFileExist $ dir </> defaultConfigFilename
      case existsConfig of
        True -> putStrLn $ "Data directory already initialized; found " ++
                (dir </> defaultConfigFilename)
        False -> do
              putStrLn $ "Directory " ++ show dir ++ " already exists"
              putStrLn $ "but it does not contain a blog config file (" ++ defaultConfigFilename ++ ")"
