module Main where

import Control.Applicative
    ( (<*>)
    , pure
    )
import Control.Monad
    ( when
    , forM_
    )
import System.IO
    ( IOMode(WriteMode)
    , openFile
    , hPutStr
    , hClose
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
    , createDirectory
    )
import System.FilePath
    ( (</>)
    )

import qualified MB.Defaults as Defaults

data Config = Config { baseDir :: FilePath
                     , postSourceDir :: FilePath
                     , htmlDir :: FilePath
                     , stylesheetDir :: FilePath
                     , postHtmlDir :: FilePath
                     , imageDir :: FilePath
                     , templateDir :: FilePath
                     }

baseDirName :: String
baseDirName = "MB_BASE_DIR"

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

generatePosts :: Config -> IO ()
generatePosts _ = return ()

generateIndex :: Config -> IO ()
generateIndex _ = return ()

generateList :: Config -> IO ()
generateList _ = return ()

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
                       }

usage :: IO ()
usage = do
  putStrLn "Usage: mb\n"
  putStrLn "mb is a tool for creating and managing a mathematically-inclined"
  putStrLn "weblog.  To use mb, you must set an environment variable to the"
  putStrLn $ "path where blog files will be stored.  Please set " ++ baseDirName
  putStrLn "and try again."

main :: IO ()
main = do
  env <- getEnvironment

  case lookup baseDirName env of
    Nothing -> usage >> exitFailure
    Just d -> do
         putStrLn $ "mb: using base directory " ++ (show d)
         let config = mkConfig d
         setup config
         generatePosts config
         generateIndex config
         generateList config