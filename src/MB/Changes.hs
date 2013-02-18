module MB.Changes
    ( anyChanges
    , summarizeChanges
    )
where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Maybe
import System.Directory (doesFileExist)
import Data.Time.Calendar
import Data.Time.Clock
import Data.List

import MB.Util
import MB.Types
import qualified MB.Files as Files

anyChanges :: ChangeSummary -> Bool
anyChanges s = or $ predicates <*> pure s
    where
      predicates = [ configChanged
                   , not . null . postsChanged
                   , not . null . templatesChanged
                   , postIndexChanged
                   , not . null . assetsChanged
                   ]

summarizeChanges :: Blog -> Bool -> IO ChangeSummary
summarizeChanges config forceAll = do
  -- Determine whether the configuration file changed.  Check to see
  -- if it's newer than the index.html file, or if no index.html
  -- exists then that's equivalent to "the config changed"
  configMtime <- getModificationTime $ configPath config
  indexExists <- doesFileExist $ Files.indexHtml config
  baseTime <- case indexExists of
                False -> do
                  t <- getCurrentTime
                  return $ t { utctDay = addDays (- 1) $ utctDay t }
                True -> getModificationTime $ Files.indexHtml config

  postIndexExists <- doesFileExist $ Files.postIndex config
  postIndexChanged' <- case postIndexExists of
                         False -> return True
                         True -> do
                            t <- getModificationTime $ Files.postIndex config
                            return $ t > baseTime

  let configChanged' = configMtime > baseTime
      needsRebuild p = forceAll || (postModificationTime p > baseTime)
      modifiedPosts = filter needsRebuild $ blogPosts config

  -- Determine whether any templates changed
  templateFiles <- dirFilenames (templateDir config)
  templateChanges <- forM templateFiles $ \f ->
                         do
                           mtime <- getModificationTime f
                           if mtime > baseTime && ".html" `isSuffixOf` f then
                               return (Just f) else
                               return Nothing

  -- Determine whether any assets changed
  assetFiles <- dirFilenamesRecursive (assetDir config)
  assetChanges <- forM assetFiles $ \f ->
                      do
                        mtime <- getModificationTime f
                        if mtime > baseTime then
                            return (Just f) else
                            return Nothing

  let baselineChanges =
          ChangeSummary { configChanged = configChanged' || forceAll
                        , postsChanged = map postFilename modifiedPosts
                        , templatesChanged = catMaybes templateChanges
                        , postIndexChanged = postIndexChanged' || forceAll
                        , assetsChanged = catMaybes assetChanges
                        }

  -- Query document processors to get other changes
  processorChanges <- forM (processors config) $ \p ->
                      case getChangeSummary p of
                        Nothing -> return noChanges
                        Just f -> f config baseTime

  -- Combine all changes
  return $ mconcat $ baselineChanges : processorChanges
