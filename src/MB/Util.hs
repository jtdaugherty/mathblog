module MB.Util
    ( copyContents
    , toUtcTime
    , loadPostIndex
    , getModificationTime
    , allPostFilenames
    , dirFilenames
    , dirFilenamesRecursive
    , serializePostIndex
    , ensureDir
    , getInlineStr
    , fromInlines
    )
where
import Control.Applicative
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , createDirectory
    , getDirectoryContents
    , copyFile
    )
import Data.Default
    ( def
    )
import System.FilePath
    ( (</>)
    , takeFileName
    , takeBaseName
    )
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Either
import Control.Monad
    ( when
    , forM_
    , forM
    , filterM
    )
import System.Posix.Files
    ( getFileStatus
    , modificationTime
    , accessTime
    , setFileTimes
    )
import System.Posix.Types
    ( EpochTime
    )
import Data.List
    ( isSuffixOf
    , isPrefixOf
    )
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Format
    ( parseTime
    )
import Data.Time.Format
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
import MB.Types
import MB.TeXMacros

-- |Given a source path SRC and a destination path DST, the SRC and
-- DST paths MUST exist and must both be directories.  All contents of
-- SRC/ will be copied, recursively, to DST/.
copyContents :: FilePath -> FilePath -> IO ()
copyContents src dst = do
  -- For each file in src, copy it to dest.
  entries <- filter (not . flip elem [".", ".."]) <$> getDirectoryContents src

  dirs <- filterM doesDirectoryExist $ map (src </>) entries
  files <- filterM doesFileExist $ map (src </>) entries

  -- For each directory in src, create it in dest, then descend into
  -- that directory in both src and dest.
  forM_ files $ \f -> do
    let destPath = dst </> takeFileName f
    st <- getFileStatus f
    copyFile f destPath
    setFileTimes destPath (accessTime st) (modificationTime st)

  forM_ dirs $ \dir ->
      do
        let dstDir = dst </> dirName
            dirName = takeFileName dir

        e <- doesDirectoryExist dstDir
        when (not e) $ do
          createDirectory dstDir
          st <- getFileStatus $ src </> dirName
          setFileTimes dstDir (accessTime st) (modificationTime st)

        copyContents (src </> dirName) dstDir

toUtcTime :: EpochTime -> UTCTime
toUtcTime t = fromJust $ parseTime defaultTimeLocale "%s" $ show t

loadPost :: FilePath -> EitherT String IO Post
loadPost fullPath = do
  fileContent <- liftIO $ readFile fullPath
  t <- liftIO $ getModificationTime fullPath
  (Pandoc.Pandoc m blocks) <- hoistEither $ case Pandoc.readMarkdown def fileContent of
      Left pandocErr -> Left $ show pandocErr
      Right v -> Right v

  -- Extract defined TeX macros in the post and store them in
  -- the post data structure to make them available to other
  -- parts of the page generation process (see Mathjax and TikZ
  -- for examples.)
  let (newBlocks, macros) = extractTeXMacros blocks

      pas = case Pandoc.docAuthors m of
              [] -> []
              as -> fromInlines <$> as

      pd = case Pandoc.docDate m of
             [] -> Nothing
             d -> Just $ fromInlines d

  tz <- liftIO getCurrentTimeZone
  localTime <- liftIO $ toLocalTime t
  let modStr = show localTime ++ "  " ++ timeZoneName tz
      bn = takeBaseName $ takeFileName fullPath
      htmlFilename =  bn ++ ".html"

  return $ Post { postTitle = Pandoc.docTitle m
                , postPath = fullPath
                , postFilename = takeFileName fullPath
                , postModificationTime = t
                , postAst = Pandoc.Pandoc m newBlocks
                , postTeXMacros = macros
                , postAuthors = pas
                , postDate = pd
                , postModificationString = modStr
                , postBaseName = bn
                , postUrl = "/posts/" ++ htmlFilename
                , postHtmlFilename = htmlFilename
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

loadPostIndex :: BlogInputFS -> EitherT String IO [Post]
loadPostIndex ifs = do
  let indexFilename = ifsPostIndexPath ifs
  e <- liftIO $ doesFileExist indexFilename

  indexNames <- liftIO $ case e of
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
  postFiles <- liftIO $ allPostFilenames $ ifsPostSourceDir ifs
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

ensureDir :: FilePath -> IO ()
ensureDir d = do
  exists <- doesDirectoryExist d
  when (not exists) $ createDirectory d

getInlineStr :: Pandoc.Inline -> String
getInlineStr (Pandoc.Str s) = s
getInlineStr (Pandoc.Math _ s) = s
getInlineStr Pandoc.Space = " "
getInlineStr i = error $ "Unexpected inline in document title, got " ++ (show i)

fromInlines :: [Pandoc.Inline] -> String
fromInlines = concat . (getInlineStr <$>)


toLocalTime :: UTCTime -> IO LocalTime
toLocalTime u = do
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz u
