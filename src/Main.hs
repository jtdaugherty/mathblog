module Main where

import Control.Applicative
    ( (<*>)
    , pure
    )
import Control.Monad
    ( when
    , forM
    , forM_
    )
import System.IO
    ( IOMode(WriteMode)
    , Handle
    , openFile
    , hPutStr
    , hClose
    , readFile
    )
import System.Exit
    ( ExitCode(ExitSuccess)
    , exitFailure
    )
import System.Environment
    ( getEnvironment
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , createDirectory
    , getDirectoryContents
    , removeFile
    , copyFile
    )
import System.FilePath
    ( (</>)
    , takeBaseName
    , takeFileName
    )
import System.Posix.Types
    ( EpochTime
    )
import System.Posix.Files
    ( getFileStatus
    , modificationTime
    , createSymbolicLink
    )
import System.Process
    ( readProcessWithExitCode
    )
import Data.List
    ( sortBy
    , isSuffixOf
    )
import Data.Maybe
    ( fromJust
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Time.Format
    ( parseTime
    )
import Data.Time.LocalTime
    ( LocalTime
    , TimeZone(timeZoneName)
    , getCurrentTimeZone
    , utcToLocalTime
    )
import System.Locale
    ( defaultTimeLocale
    )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as Pandoc

import qualified MB.Defaults as Defaults

data Config = Config { baseDir :: FilePath
                     , postSourceDir :: FilePath
                     , htmlDir :: FilePath
                     , stylesheetDir :: FilePath
                     , postHtmlDir :: FilePath
                     , postIntermediateDir :: FilePath
                     , imageDir :: FilePath
                     , templateDir :: FilePath
                     , htmlTempDir :: FilePath
                     }

data Post = Post { postTitle :: Int -> String
                 , postFilename :: String -- relative to the postSourceDir
                 , postModificationTime :: UTCTime
                 , postAst :: Pandoc.Pandoc
                 }

baseDirEnvName :: String
baseDirEnvName = "MB_BASE_DIR"

indexHtml :: Config -> FilePath
indexHtml c = htmlDir c </> "index.html"

listHtml :: Config -> FilePath
listHtml c = htmlDir c </> "list.html"

listHtex :: Config -> FilePath
listHtex c = htmlTempDir c </> "list.htex"

listTmpHtml :: Config -> FilePath
listTmpHtml c = htmlTempDir c </> "list.html"

firstPost :: Config -> FilePath
firstPost c = postSourceDir c </> "first-post.txt"

pagePreamble :: Config -> FilePath
pagePreamble c = templateDir c </> "pagePreamble.html"

pagePostamble :: Config -> FilePath
pagePostamble c = templateDir c </> "pagePostamble.html"

postPreamble :: Config -> FilePath
postPreamble c = templateDir c </> "postPreamble.html"

postPostamble :: Config -> FilePath
postPostamble c = templateDir c </> "postPostamble.html"

stylesheet :: Config -> FilePath
stylesheet c = stylesheetDir c </> "stylesheet.css"

safeMakeDir :: FilePath -> IO ()
safeMakeDir dir = do
  exists <- doesDirectoryExist dir
  when (not exists) $ do
                    putStrLn $ "Creating directory: " ++ (show dir)
                    createDirectory dir

safeCreateFile :: FilePath -> String -> IO ()
safeCreateFile path contents = do
  exists <- doesFileExist path
  when (not exists) $ do
    putStrLn $ "Creating default file: " ++ (show path)
    h <- openFile path WriteMode
    hPutStr h contents
    hClose h

--- xxx need to extract the document title, possibly making some
--- assumptions about valid titles
title :: Pandoc.Pandoc -> Int -> String
title (Pandoc.Pandoc m _) dpi = concat $ map getStr $ Pandoc.docTitle m
    where
      getStr (Pandoc.Str s) = s
      getStr (Pandoc.Math _ s) = "<EQ DPI=\"" ++ show dpi ++ "\">" ++ s ++ "</EQ>"
      getStr Pandoc.Space = " "
      getStr i = error $ "Unexpected inline in document title, got " ++ (show i)

toUtcTime :: EpochTime -> UTCTime
toUtcTime t = fromJust $ parseTime defaultTimeLocale "%s" $ show t

toLocalTime :: UTCTime -> IO LocalTime
toLocalTime u = do
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz u

allPosts :: Config -> IO [Post]
allPosts config = do
  -- Read all files from the post source directory (except .. and .)
  allFiles <- getDirectoryContents $ postSourceDir config
  let postFiles = [ f | f <- allFiles
                  , ".txt" `isSuffixOf` f
                  ]

  -- For each file, construct a Post from it.
  posts <- forM postFiles $
           \f -> do
             let fullPath = postSourceDir config </> f
             info <- getFileStatus fullPath
             fileContent <- readFile fullPath
             let doc = Pandoc.readMarkdown Pandoc.defaultParserState fileContent
                 t = toUtcTime $ modificationTime info

             return $ Post { postTitle = title doc
                           , postFilename = fullPath
                           , postModificationTime = t
                           , postAst = doc
                           }

  -- Return posts sorted by modification time, descending
  return $ sortBy (\a b -> postModificationTime a `compare` postModificationTime b) posts

pandocWriterOptions :: Pandoc.WriterOptions
pandocWriterOptions =
    Pandoc.defaultWriterOptions { Pandoc.writerHTMLMathMethod = Pandoc.GladTeX
                                }

postBaseName :: Post -> String
postBaseName = takeBaseName . takeFileName . postFilename

postHtex :: Config -> Post -> String
postHtex config p = htmlTempDir config </> postBaseName p ++ ".htex"

gladTex :: Config -> FilePath -> String -> IO ()
gladTex config htexPath color = do
  let args = [ "-d"
             , imageDir config
             , "-u"
             , "/images/"
             , "-r"
             , "120"
             , "-s"
             , "10"
             , "-b"
             , "FFFFFF"
             , "-c"
             , color
             , htexPath
             ]

  (ecode, _, err) <- readProcessWithExitCode "gladtex" args ""

  when (ecode /= ExitSuccess) $ do
    putStrLn $ "Error processing " ++ (show htexPath) ++ " with gladtex:"
    putStrLn err
    exitFailure

writePost :: Handle -> Post -> IO ()
writePost h post = do
  hPutStr h $ "<h1>" ++ postTitle post 175 ++ "</h1>"
  hPutStr h $ Pandoc.writeHtmlString pandocWriterOptions (postAst post)

postIntermediateHtml :: Config -> Post -> FilePath
postIntermediateHtml config post = postIntermediateDir config </> postBaseName post ++ ".html"

postFinalHtml :: Config -> Post -> FilePath
postFinalHtml config p = postHtmlDir config </> postBaseName p ++ ".html"

buildPost :: Handle -> Config -> Post -> IO ()
buildPost h config post = do
  hPutStr h =<< (readFile $ pagePreamble config)
  hPutStr h =<< (readFile $ postPreamble config)
  hPutStr h =<< (readFile $ postIntermediateHtml config post)
  hPutStr h =<< (readFile $ postPostamble config)
  hPutStr h =<< (readFile $ pagePostamble config)

generatePost :: Config -> Post -> IO ()
generatePost config post = do
  let tempHtml = htmlTempDir config </> postBaseName post ++ ".html"
      finalHtml = postIntermediateHtml config post

  htmlExists <- doesFileExist finalHtml
  skip <- case htmlExists of
            False -> return False
            True -> do
              info <- getFileStatus finalHtml
              return $ (toUtcTime $ modificationTime info) > postModificationTime post

  when (not skip) $ do
    putStrLn $ "Processing: " ++ postBaseName post

    h <- openFile (postHtex config post) WriteMode
    writePost h post
    hClose h

    -- Run gladtex on the temp file to generate the final file.
    gladTex config (postHtex config post) "000000"

    -- Gladtex generates the HTML in the same directory as the source
    -- file, so we need to copy that to the final location.
    copyFile tempHtml finalHtml

    -- Remove the temporary file.
    removeFile $ postHtex config post
    removeFile tempHtml

generatePosts :: Config -> [Post] -> IO ()
generatePosts config posts =
    forM_ posts $ \p -> do
      generatePost config p

      h <- openFile (postFinalHtml config p) WriteMode
      buildPost h config p
      hClose h

generateIndex :: Config -> Post -> IO ()
generateIndex config post = do
  let dest = postFinalHtml config post
      index = indexHtml config

  exists <- doesFileExist index
  when exists $ removeFile index

  createSymbolicLink dest index

generateList :: Config -> [Post] -> IO ()
generateList config posts = do
  putStrLn "Generating all-posts list."

  tz <- getCurrentTimeZone
  h <- openFile (listHtex config) WriteMode

  hPutStr h =<< (readFile $ pagePreamble config)
  hPutStr h "<div id=\"all-posts\">"

  -- For each post in the order they were given, extract the
  -- unrendered title and construct an htex document.  Then render it
  -- to the listing location.
  forM_ posts $ \p -> do
    localTime <- toLocalTime $ postModificationTime p
    hPutStr h $ concat [ "<div class=\"listing-entry\"><span class=\"post-title\">"
                       , "<a href=\"" ++ postUrl p ++ "\">"
                       , postTitle p 110
                       , "</a></span><span class=\"post-created\">Posted "
                       , show localTime
                       , " "
                       , timeZoneName tz
                       , "</span></div>\n"
                       ]

  hPutStr h "</div>"
  hPutStr h =<< (readFile $ pagePostamble config)
  hClose h

  gladTex config (listHtex config) "0000FF"

  -- Gladtex generates the HTML in the same directory as the source
  -- file, so we need to copy that to the final location.
  copyFile (listTmpHtml config) (listHtml config)

  -- Remove the temporary file.
  removeFile $ listHtex config
  removeFile $ listTmpHtml config

postUrl :: Post -> String
postUrl p = "/posts/" ++ postBaseName p ++ ".html"

setup :: Config -> IO ()
setup config = do
  -- If the base directory doesn't already exist, create it.
  let creationOrder = [ baseDir
                      , postSourceDir
                      , htmlDir
                      , stylesheetDir
                      , postHtmlDir
                      , postIntermediateDir
                      , imageDir
                      , templateDir
                      , htmlTempDir
                      ]

  mapM_ safeMakeDir $ creationOrder <*> pure config

  -- Install default files.
  let files = [ (firstPost, Defaults.firstPost)
              , (pagePreamble, Defaults.pagePreamble)
              , (pagePostamble, Defaults.pagePostamble)
              , (postPreamble, Defaults.postPreamble)
              , (postPostamble, Defaults.postPostamble)
              , (stylesheet, Defaults.stylesheet)
              ]

  forM_ files $ \(path, content) -> do
                 safeCreateFile (path config) content

  -- Generated files, such as the index and post list, will be
  -- generated as usual.

  return ()

mkConfig :: FilePath -> Config
mkConfig base = Config { baseDir = base
                       , postSourceDir = base </> "posts"
                       , htmlDir = base </> "html"
                       , stylesheetDir = base </> "html" </> "stylesheets"
                       , postHtmlDir = base </> "html" </> "posts"
                       , postIntermediateDir = base </> "generated"
                       , imageDir = base </> "html" </> "images"
                       , templateDir = base </> "templates"
                       , htmlTempDir = base </> "temp"
                       }

usage :: IO ()
usage = do
  putStrLn "Usage: mb\n"
  putStrLn "mb is a tool for creating and managing a mathematically-inclined"
  putStrLn "weblog.  To use mb, you must set an environment variable to the"
  putStrLn $ "path where blog files will be stored.  Please set " ++ baseDirEnvName
  putStrLn "and try again."

main :: IO ()
main = do
  env <- getEnvironment

  -- XXX look for gladtex in path, put this in the configuration

  case lookup baseDirEnvName env of
    Nothing -> usage >> exitFailure
    Just d -> do
         when (head d /= '/') $ do
                putStrLn $ baseDirEnvName ++ " must contain an absolute path"
                exitFailure

         putStrLn $ "mb: using base directory " ++ (show d)
         let config = mkConfig d
         setup config

         posts <- allPosts config

         generatePosts config posts
         generateIndex config $ head posts
         generateList config posts

         putStrLn "Done."