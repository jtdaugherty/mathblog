module MB.Processors.Base
    ( baseProcessor
    )
where

import Control.Applicative
import Control.Monad.Trans

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

  liftIO $ copyContents assets outputDir
