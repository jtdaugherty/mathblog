module MB.Base
    ( baseProcessor
    )
where

import Control.Applicative
import Control.Monad
import System.FilePath
import System.Directory

import MB.Types
import MB.Util

baseProcessor :: Processor
baseProcessor =
    nullProcessor { installAssets = Just doInstallAssets
                  }

doInstallAssets :: Blog -> IO ()
doInstallAssets blog = do
  let ad = assetDir blog

  -- For each file and directory in assets/, copy it to the output
  -- directory.
  entries <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents ad

  dirs <- filterM doesDirectoryExist $ map (ad </>) entries
  files <- filterM doesFileExist $ map (ad </>) entries

  forM_ dirs $ \d -> copyTree d (htmlDir blog </> (takeBaseName d))
  forM_ files $ \f -> copyFile f (htmlDir blog)
