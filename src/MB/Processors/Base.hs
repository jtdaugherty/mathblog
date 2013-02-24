module MB.Processors.Base
    ( baseProcessor
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.FilePath
import System.Directory

import MB.Types
import MB.Util

baseProcessor :: Processor
baseProcessor =
    nullProcessor { installAssets = Just doInstallAssets
                  }

doInstallAssets :: BlogM ()
doInstallAssets = do
  assets <- ifsAssetDir <$> inputFS <$> theBlog
  outputDir <- ofsBaseDir <$> outputFS <$> theBlog

  -- For each file and directory in assets/, copy it to the output
  -- directory.
  liftIO $ do
    entries <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents assets

    files <- filterM doesFileExist $ (assets </>) <$> entries
    forM_ files $ \f -> copyFile f (outputDir </> (takeFileName f))

    dirs <- filterM doesDirectoryExist $ (assets </>) <$> entries
    forM_ dirs $ \d -> copyTree d (outputDir </> (takeBaseName d))
