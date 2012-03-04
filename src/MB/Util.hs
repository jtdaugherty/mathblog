module MB.Util
    ( copyTree
    , toUtcTime
    , loadPostIndex
    , getModificationTime
    , allPostFilenames
    , dirFilenames
    , anyChanges
    , summarizeChanges
    , serializePostIndex
    )
where
import Control.Applicative
    ( (<$>)
    , (<*>)
    , pure
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , createDirectory
    , getDirectoryContents
    , copyFile
    )
import System.FilePath
    ( (</>)
    , takeFileName
    )
import Control.Monad
    ( when
    , forM_
    , forM
    , filterM
    )
import System.Exit
    ( exitFailure
    )
import System.Posix.Files
    ( getFileStatus
    , modificationTime
    )
import System.Posix.Types
    ( EpochTime
    )
import Data.List
    ( isSuffixOf
    , isPrefixOf
    )
import Data.Monoid
import Data.Time.Clock
    ( UTCTime(utctDay)
    , getCurrentTime
    )
import Data.Time.Calendar
    ( addDays
    )
import Data.Time.Format
    ( parseTime
    )
import System.Locale
    ( defaultTimeLocale
    )
import System.IO
    ( IOMode(ReadMode)
    , openFile
    , hClose
    , hGetContents
    )
import Data.List
    ( sortBy
    )
import Data.Maybe
    ( fromJust
    , catMaybes
    )
import qualified Text.Pandoc as Pandoc
import qualified MB.Files as Files
import MB.Types

copyTree :: FilePath -> FilePath -> IO ()
copyTree srcPath dstPath = do
  dstFExists <- doesFileExist dstPath
  dstDExists <- doesDirectoryExist dstPath

  when (dstFExists) $ do
    putStrLn $ "Cannot copy " ++ (show srcPath) ++ " to existing destination path " ++
                 (show dstPath) ++ "; remove to continue."
    exitFailure

  when (not dstDExists) $ createDirectory dstPath

  copyTree' srcPath dstPath

  where
    copyTree' src dst = do
      -- For each file in src, copy it to dest.
      entries <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents src

      dirs <- filterM doesDirectoryExist $ map (src </>) entries
      files <- filterM doesFileExist $ map (src </>) entries

      -- For each directory in src, create it in dest, then descend
      -- into that directory in both src and dest.
      forM_ files $ \f -> copyFile f $ dst </> takeFileName f
      forM_ dirs $ \dir ->
          do
            let dstDir = dst </> dirName
                dirName = takeFileName dir

            e <- doesDirectoryExist dstDir
            when (not e) $ createDirectory dstDir
            copyTree' (src </> dirName) dstDir

toUtcTime :: EpochTime -> UTCTime
toUtcTime t = fromJust $ parseTime defaultTimeLocale "%s" $ show t

loadPost :: FilePath -> IO Post
loadPost fullPath = do
  fileContent <- readFile fullPath
  t <- getModificationTime fullPath
  let doc = Pandoc.readMarkdown Pandoc.defaultParserState fileContent
      Pandoc.Pandoc m _ = doc

  return $ Post { postTitle = Pandoc.docTitle m
                , postPath = fullPath
                , postFilename = takeFileName fullPath
                , postModificationTime = t
                , postAst = doc
                }

dirFilenames :: FilePath -> IO [FilePath]
dirFilenames dir = do
  allFiles <- getDirectoryContents dir
  return [ dir </> f | f <- allFiles
         , not $ "." `isPrefixOf` f
         ]

dirFilenamesRecursive :: FilePath -> IO [FilePath]
dirFilenamesRecursive dir = do
  entries <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents dir
  dirs <- filterM doesDirectoryExist $ map (dir </>) entries
  files <- filterM doesFileExist $ map (dir </>) entries

  rest <- concat <$> forM dirs dirFilenamesRecursive
  return $ files ++ rest

allPostFilenames :: FilePath -> IO [FilePath]
allPostFilenames postSrcDir = do
  allFiles <- dirFilenames postSrcDir
  return [ f | f <- allFiles
         , ".txt" `isSuffixOf` f
         ]

getModificationTime :: FilePath -> IO UTCTime
getModificationTime fullPath = do
  info <- getFileStatus fullPath
  return $ toUtcTime $ modificationTime info

loadPostIndex :: FilePath -> IO [Post]
loadPostIndex postSrcDir = do
  let indexFilename = postSrcDir </> "posts-index"
  e <- doesFileExist indexFilename

  indexNames <- case e of
                  False -> return []
                  True -> do
                         h <- openFile indexFilename ReadMode
                         s <- hGetContents h
                         s `seq` return ()
                         let idx = unserializePostIndex s
                         hClose h
                         return idx

  -- Now that we have a postIndex to deal with, load posts from disk
  -- and insert them into the post index in the proper order

  postFiles <- allPostFilenames postSrcDir
  posts <- mapM loadPost postFiles

  -- There are two types of posts to put into the index: the ones that
  -- are not already in the index, and the ones that are (and in a
  -- specific order).

  let pairs = [ (postFilename p, p) | p <- posts ]
      newPosts = [ p | p <- posts, not $ postFilename p `elem` indexNames ]
      preexistingPosts = catMaybes [ lookup n pairs | n <- indexNames ]
      ps = sortPosts newPosts ++ preexistingPosts

  return ps

serializePostIndex :: [Post] -> String
serializePostIndex ps = unlines $ map postFilename ps

unserializePostIndex :: String -> [String]
unserializePostIndex = lines

sortPosts :: [Post] -> [Post]
sortPosts = sortBy (\a b -> postModificationTime b `compare`
                            postModificationTime a)

anyChanges :: ChangeSummary -> Bool
anyChanges s = or $ predicates <*> pure s
    where
      predicates = [ configChanged
                   , not . null . postsChanged
                   , templatesChanged
                   , postIndexChanged
                   , assetsChanged
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
  templateChanges <- forM templateFiles $ \f -> do
                            mtime <- getModificationTime f
                            return $ mtime > baseTime

  -- Determine whether any assets changed
  assetFiles <- dirFilenamesRecursive (assetDir config)
  assetChanges <- forM assetFiles $ \f -> do
                            mtime <- getModificationTime f
                            return $ mtime > baseTime

  let baselineChanges =
          ChangeSummary { configChanged = configChanged' || forceAll
                        , postsChanged = map postFilename modifiedPosts
                        , templatesChanged = or (forceAll : templateChanges)
                        , postIndexChanged = postIndexChanged' || forceAll
                        , assetsChanged = or (forceAll : assetChanges)
                        }

  -- Query document processors to get other changes
  processorChanges <- forM (processors config) $ \p ->
                      case getChangeSummary p of
                        Nothing -> return noChanges
                        Just f -> f config baseTime

  -- Combine all changes
  return $ mconcat $ baselineChanges : processorChanges
