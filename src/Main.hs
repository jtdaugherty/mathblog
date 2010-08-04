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
    ( exitFailure
    )
import System.Environment
    ( getEnvironment
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , getDirectoryContents
    , removeFile
    , copyFile
    )
import System.FilePath
    ( (</>)
    )
import System.Posix.Files
    ( getFileStatus
    , modificationTime
    , createSymbolicLink
    )
import Data.List
    ( sortBy
    , isSuffixOf
    )
import Data.Maybe
    ( isNothing
    )
import Data.Time.LocalTime
    ( TimeZone(timeZoneName)
    , getCurrentTimeZone
    )
import qualified Text.Pandoc as Pandoc
import MB.Util
    ( copyTree
    , toUtcTime
    , toLocalTime
    , pandocTitle
    )
import MB.Gladtex
    ( gladTex
    , checkForGladtex
    )
import MB.Types
import qualified MB.Files as Files

skelDir :: FilePath
skelDir = "/home/cygnus/src/mathblog/skel"

baseDirEnvName :: String
baseDirEnvName = "MB_BASE_DIR"

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

             return $ Post { postTitle = pandocTitle doc
                           , postFilename = fullPath
                           , postModificationTime = t
                           , postAst = doc
                           }

  -- Return posts sorted by modification time, descending
  return $ sortBy (\a b -> postModificationTime b `compare` postModificationTime a) posts

pandocWriterOptions :: Pandoc.WriterOptions
pandocWriterOptions =
    Pandoc.defaultWriterOptions { Pandoc.writerHTMLMathMethod = Pandoc.GladTeX
                                }

writePost :: Handle -> Post -> IO ()
writePost h post = do
  created <- postModificationString post
  hPutStr h $ "<h1>" ++ postTitle post 175 ++ "</h1>"
  hPutStr h $ "<span class=\"post-created\">Posted " ++ created ++ "</span>"
  hPutStr h $ Pandoc.writeHtmlString pandocWriterOptions (postAst post)

buildLinks :: Maybe Post -> Maybe Post -> String
buildLinks prev next =
    "<div id=\"prev-next-links\">"
      ++ link "next-link" "older" next
      ++ link "prev-link" "newer" prev
      ++ "</div>"
        where
          link cls name Nothing =
              "<span class=\"" ++ cls ++ "-subdued\">" ++ name ++ "</span>"
          link cls name (Just p) =
              "<a class=\"" ++ cls ++ "\" href=\"" ++ Files.postUrl p ++
                                "\">" ++ name ++ "</a>"

jsInfo :: Post -> String
jsInfo post =
    "<script type=\"text/javascript\">\n\
    \Blog = {\n\
    \  pageName: " ++ show (Files.postBaseName post) ++
    "\n\
    \};\n\
    \</script>\n"

buildPost :: Handle -> Config -> Post -> (Maybe Post, Maybe Post) -> IO ()
buildPost h config post prevNext = do
  hPutStr h =<< (readFile $ Files.pagePreamble config)
  hPutStr h $ jsInfo post
  hPutStr h $ uncurry buildLinks prevNext
  hPutStr h =<< (readFile $ Files.postPreamble config)
  hPutStr h =<< (readFile $ Files.postIntermediateHtml config post)
  hPutStr h =<< (readFile $ Files.postPostamble config)
  hPutStr h =<< (readFile $ Files.pagePostamble config)

generatePost :: Config -> Post -> IO ()
generatePost config post = do
  let tempHtml = htmlTempDir config </> Files.postBaseName post ++ ".html"
      finalHtml = Files.postIntermediateHtml config post

  htmlExists <- doesFileExist finalHtml
  skip <- case htmlExists of
            False -> return False
            True -> do
              info <- getFileStatus finalHtml
              return $ (toUtcTime $ modificationTime info) > postModificationTime post

  when (not skip) $ do
    putStrLn $ "Processing: " ++ Files.postBaseName post

    h <- openFile (Files.postHtex config post) WriteMode
    writePost h post
    hClose h

    -- Run gladtex on the temp file to generate the final file.
    gladTex config (Files.postHtex config post) "000000"

    -- Gladtex generates the HTML in the same directory as the source
    -- file, so we need to copy that to the final location.
    copyFile tempHtml finalHtml

    -- Remove the temporary file.
    removeFile $ Files.postHtex config post
    removeFile tempHtml

generatePosts :: Config -> [Post] -> IO ()
generatePosts config posts = do
  let n = length posts
  forM_ (zip posts [0..]) $ \(p, i) ->
      do
        let prevPost = if i == 0 then Nothing else Just (posts !! (i - 1))
            nextPost = if i == n - 1 then Nothing else Just (posts !! (i + 1))

        generatePost config p
        h <- openFile (Files.postFinalHtml config p) WriteMode
        buildPost h config p (prevPost, nextPost)
        hClose h

generateIndex :: Config -> Post -> IO ()
generateIndex config post = do
  let dest = Files.postFinalHtml config post
      index = Files.indexHtml config

  exists <- doesFileExist index
  when exists $ removeFile index

  createSymbolicLink dest index

postModificationString :: Post -> IO String
postModificationString p = do
  tz <- getCurrentTimeZone
  localTime <- toLocalTime $ postModificationTime p
  return $ show localTime ++ "  " ++ timeZoneName tz

generateList :: Config -> [Post] -> IO ()
generateList config posts = do
  putStrLn "Generating all-posts list."

  h <- openFile (Files.listHtex config) WriteMode

  hPutStr h =<< (readFile $ Files.pagePreamble config)
  hPutStr h "<div id=\"all-posts\">"

  -- For each post in the order they were given, extract the
  -- unrendered title and construct an htex document.  Then render it
  -- to the listing location.
  forM_ posts $ \p -> do
    created <- postModificationString p
    hPutStr h $ concat [ "<div class=\"listing-entry\"><span class=\"post-title\">"
                       , "<a href=\"" ++ Files.postUrl p ++ "\">"
                       , postTitle p 110
                       , "</a></span><span class=\"post-created\">Posted "
                       , created
                       , "</span></div>\n"
                       ]

  hPutStr h "</div>"
  hPutStr h =<< (readFile $ Files.pagePostamble config)
  hClose h

  gladTex config (Files.listHtex config) "0000FF"

  -- Gladtex generates the HTML in the same directory as the source
  -- file, so we need to copy that to the final location.
  copyFile (Files.listTmpHtml config) (Files.listHtml config)

  -- Remove the temporary file.
  removeFile $ Files.listHtex config
  removeFile $ Files.listTmpHtml config

setup :: Config -> IO ()
setup config = do
  exists <- doesDirectoryExist $ baseDir config

  when (not exists) $ do
          putStrLn $ "Setting up data directory using skeleton: " ++ skelDir
          copyTree skelDir $ baseDir config

  validate config

validate :: Config -> IO ()
validate config = do
  let dirs = [ baseDir
             , postSourceDir
             , htmlDir
             , stylesheetDir
             , postHtmlDir
             , postIntermediateDir
             , imageDir
             , templateDir
             , htmlTempDir
             ]

  forM_ (dirs <*> pure config) $ \d ->
      do
        exists <- doesDirectoryExist d
        when (not exists) $
             do
               putStrLn $ "Required directory missing: " ++ (show d)
               exitFailure

mkConfig :: FilePath -> Config
mkConfig base = Config { baseDir = base
                       , postSourceDir = base </> "posts"
                       , htmlDir = base </> "html"
                       , stylesheetDir = base </> "html" </> "stylesheets"
                       , postHtmlDir = base </> "html" </> "posts"
                       , postIntermediateDir = base </> "generated"
                       , imageDir = base </> "html" </> "generated-images"
                       , templateDir = base </> "templates"
                       , htmlTempDir = base </> "tmp"
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

  checkForGladtex

  let mBase = lookup baseDirEnvName env

  when (isNothing mBase) $ usage >> exitFailure

  let Just dir = mBase
  when (head dir /= '/') $ do
         putStrLn $ baseDirEnvName ++ " must contain an absolute path"
         exitFailure

  putStrLn $ "mb: using base directory " ++ (show dir)
  let config = mkConfig dir
  setup config

  posts <- allPosts config

  generatePosts config posts
  generateIndex config $ head posts
  generateList config posts

  putStrLn "Done."