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
    , openFile
    , hPutStr
    , hClose
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
    )
import System.Process
    ( readProcessWithExitCode
    )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as Pandoc

import qualified MB.Defaults as Defaults

data Config = Config { baseDir :: FilePath
                     , postSourceDir :: FilePath
                     , htmlDir :: FilePath
                     , stylesheetDir :: FilePath
                     , postHtmlDir :: FilePath
                     , imageDir :: FilePath
                     , templateDir :: FilePath
                     , htmlTempDir :: FilePath
                     }

data Post = Post { postTitle :: String
                 , postFilename :: String -- relative to the postSourceDir
                 , postModificationTime :: EpochTime
                 , postAst :: Pandoc.Pandoc
                 }
            deriving (Show)

baseDirEnvName :: String
baseDirEnvName = "MB_BASE_DIR"

indexHtml :: Config -> FilePath
indexHtml c = htmlDir c </> "index.html"

listHtml :: Config -> FilePath
listHtml c = htmlDir c </> "list.html"

firstPost :: Config -> FilePath
firstPost c = postSourceDir c </> "first-post.txt"

preamble :: Config -> FilePath
preamble c = templateDir c </> "preamble.html"

postamble :: Config -> FilePath
postamble c = templateDir c </> "postamble.html"

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
title :: Pandoc.Pandoc -> String
title (Pandoc.Pandoc m _) = concat $ map getStr $ Pandoc.docTitle m
    where
      getStr (Pandoc.Str s) = s
      getStr Pandoc.Space = " "
      getStr i = error $ "Unexpected inline in document title, got " ++ (show i)

allPosts :: Config -> IO [Post]
allPosts config = do
  -- Read all files from the post source directory (except .. and .)
  allFiles <- getDirectoryContents $ postSourceDir config
  let postFiles = [ f | f <- allFiles, not $ f `elem` ["..", "."] ]

  -- For each file, construct a Post from it.
  forM postFiles $
           \f -> do
             let fullPath = postSourceDir config </> f
             info <- getFileStatus fullPath
             fileContent <- readFile fullPath
             let doc = Pandoc.readMarkdown Pandoc.defaultParserState fileContent

             return $ Post { postTitle = title doc
                           , postFilename = fullPath
                           , postModificationTime = modificationTime info
                           , postAst = doc
                           }

pandocWriterOptions :: Pandoc.WriterOptions
pandocWriterOptions =
    Pandoc.defaultWriterOptions { Pandoc.writerHTMLMathMethod = Pandoc.GladTeX
                                }

postBaseName :: Post -> String
postBaseName = takeBaseName . takeFileName . postFilename

postHtex :: Config -> Post -> String
postHtex config p = htmlTempDir config </> postBaseName p ++ ".htex"

gladTex :: Config -> Post -> IO ()
gladTex config p = do
  let args = [ "-d"
             , imageDir config
             , "-u"
             , "/images/"
             , postHtex config p
             ]

  (ecode, _, err) <- readProcessWithExitCode "gladtex" args ""

  when (ecode /= ExitSuccess) $ do
    putStrLn $ "Error processing " ++ (show $ postHtex config p) ++ " with gladtex:"
    putStrLn err
    exitFailure

generatePost :: Config -> Post -> IO ()
generatePost config post = do
  let tempHtml = htmlTempDir config </> postBaseName post ++ ".html"
      finalHtml = postHtmlDir config </> postBaseName post ++ ".html"

  htmlExists <- doesFileExist finalHtml
  skip <- case htmlExists of
            False -> return False
            True -> do
              info <- getFileStatus finalHtml
              return $ modificationTime info > postModificationTime post

  when (not skip) $ do
    putStrLn $ "Processing: " ++ postBaseName post

    h <- openFile (postHtex config post) WriteMode
    hPutStr h $ Pandoc.writeHtmlString pandocWriterOptions (postAst post)
    hClose h

    -- Run gladtex on the temp file to generate the final file.
    gladTex config post

    -- Gladtex generates the HTML in the same directory as the source
    -- file, so we need to copy that to the final location.
    copyFile tempHtml finalHtml

    -- Remove the temporary file.
    removeFile $ postHtex config post
    removeFile tempHtml

generatePosts :: Config -> [Post] -> IO ()
generatePosts config posts =
    forM_ posts $ \p -> generatePost config p

generateIndex :: Config -> Post -> IO ()
generateIndex _ _ = return ()

generateList :: Config -> [Post] -> IO ()
generateList _ _ = return ()

setup :: Config -> IO ()
setup config = do
  -- If the base directory doesn't already exist, create it.
  let creationOrder = [ baseDir
                      , postSourceDir
                      , htmlDir
                      , stylesheetDir
                      , postHtmlDir
                      , imageDir
                      , templateDir
                      , htmlTempDir
                      ]

  mapM_ safeMakeDir $ creationOrder <*> pure config

  -- Install default files.
  let files = [ (firstPost, Defaults.firstPost)
              , (preamble, Defaults.preamble)
              , (postamble, Defaults.postamble)
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
                putStrLn $ baseDirEnvName ++ " must contain an abosolute path"
                exitFailure

         putStrLn $ "mb: using base directory " ++ (show d)
         let config = mkConfig d
         setup config

         posts <- allPosts config

         generatePosts config posts
         generateIndex config $ head posts
         generateList config posts

         putStrLn "Done."