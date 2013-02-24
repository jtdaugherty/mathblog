module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock
import Data.List
import System.Exit
import System.Directory
    hiding (getModificationTime)
import System.FilePath

import System.FSNotify
import qualified Filesystem.Path.CurrentOS as FP

import qualified MB.Config as Config
import qualified MB.Files as Files
import MB.Util
import MB.Types
import MB.Startup
import MB.Initialize
import MB.Templates

import MB.Gen.Post
import MB.Gen.PostList
import MB.Gen.Index
import MB.Gen.RSS

import MB.Processors.Tikz
import MB.Processors.Mathjax
import MB.Processors.Base

ensureDirs :: Blog -> IO ()
ensureDirs blog = do
  let dirs = [ postSourceDir
             , htmlDir
             , assetDir
             , postHtmlDir
             , imageDir
             , templateDir
             , htmlTempDir
             ]

  forM_ (dirs <*> pure blog) ensureDir

doGeneration :: StartupConfig -> (GenEvent -> IO ()) -> IO ()
doGeneration config handler = do
  ch <- newChan
  mv <- newEmptyMVar

  let waitForEvents = do
        ev <- readChan ch
        handler ev
        case ev of
          Finished -> putMVar mv ()
          _ -> waitForEvents

  _ <- forkIO waitForEvents
  blog <- mkBlog config
  runBlogM blog ch config regenerateContent

  -- Signal event handler to shut down
  writeChan ch Finished

  -- Wait for event handler to shut down
  _ <- readMVar mv
  return ()

isEventInteresting :: StartupConfig -> Event -> Bool
isEventInteresting conf ev =
    let fp = case ev of
               Added f _ -> f
               Modified f _ -> f
               Removed f _ -> f

        isPost f = (dataDirectory conf </> "posts/") `isPrefixOf` f &&
                   ".txt" `isSuffixOf` f &&
                   not ("." `isPrefixOf` takeFileName f)

        isPostIndex f = (dataDirectory conf </> "posts/post-index") == f

        isAsset f = (dataDirectory conf </> "assets/") `isPrefixOf` f

        isTemplate f = (dataDirectory conf </> "templates/") `isPrefixOf` f &&
                       ".html" `isSuffixOf` f

        isConfig f = (dataDirectory conf </> "blog.cfg") == f

    in or ([ isPost
           , isAsset
           , isTemplate
           , isConfig
           , isPostIndex
           ] <*> pure (FP.encodeString fp))

scanForChanges :: StartupConfig -> (GenEvent -> IO ()) -> IO ()
scanForChanges conf h = do
  forever $ do
    withManager $ \m ->
        do
          ch <- newChan
          watchTreeChan m (FP.decodeString $ dataDirectory conf) (const True) ch
          let nextEv = do
                ev <- readChan ch
                if isEventInteresting conf ev then return ev else nextEv
          evt <- nextEv
          case evt of
            Added fp _ -> putStrLn $ "File created: " ++ FP.encodeString fp
            Modified fp _ -> putStrLn $ "File modified: " ++ FP.encodeString fp
            Removed fp _ -> putStrLn $ "File removed: " ++ FP.encodeString fp
          doGeneration conf h
          putStrLn ""
          threadDelay $ 500 * 1000

mathBackends :: [(String, Processor)]
mathBackends =
    [ ("mathjax", mathjaxProcessor)
    ]

eqBackends :: [(String, Processor)]
eqBackends =
    [ ("tikz", tikzProcessor)
    ]

mkBlog :: StartupConfig -> IO Blog
mkBlog conf = do
  let base = dataDirectory conf
      configFile = base </> (configFilePath conf)
  e <- doesFileExist configFile

  when (not e) $ do
                  putStrLn $ "Configuration file " ++ configFile ++ " not found"
                  exitFailure

  let requiredValues = [ "baseUrl"
                       , "title"
                       , "authorName"
                       , "authorEmail"
                       ]

  cfg <- Config.readConfig configFile requiredValues

  let Just cfg_baseUrl = lookup "baseUrl" cfg
      Just cfg_title = lookup "title" cfg
      Just cfg_authorName = lookup "authorName" cfg
      Just cfg_authorEmail = lookup "authorEmail" cfg

  -- Load blog posts from disk
  let postSrcDir = base </> "posts"
  allPosts <- loadPostIndex postSrcDir

  let requestedMathBackend = lookup "mathBackend" cfg
      mathBackend = case requestedMathBackend of
                      Nothing -> mathjaxProcessor
                      Just b -> case lookup b mathBackends of
                                  Nothing -> error $ "Unsupported math backend " ++ show b
                                             ++ "; valid choices are "
                                             ++ (show $ fst <$> mathBackends)
                                  Just proc -> proc

      eqBackendConfig = catMaybes $ isBackendRequested <$> eqBackends
      isBackendRequested (nam, p) =
          let Just opt = lookup nam cfg <|> Just "no"
          in if Config.affirmative opt
             then Just p
             else Nothing

      procs = baseProcessor : eqBackendConfig ++ [mathBackend]

  let html = htmlOutputDirectory conf

  -- Get modification times.
  indexMod <- do
    let pth = postSrcDir </> "posts-index"
    ex <- doesFileExist pth
    if ex then getModificationTime pth else getCurrentTime

  configMod <- getModificationTime configFile

  -- Get the most recent page template modification time.
  let templatePath p = base </> "templates" </> p
  tmpls <- filter (not . flip elem [".", ".."]) <$> (getDirectoryContents $ base </> "templates")
  tmplTime <- last <$> sort <$> mapM getModificationTime (templatePath <$> tmpls)

  baseline <- do
    let indexHtml = html </> "index.html"
    ex <- doesFileExist indexHtml
    if ex then getModificationTime indexHtml else getCurrentTime

  let b = Blog { baseDir = base
               , postSourceDir = postSrcDir
               , htmlDir = html
               , assetDir = base </> "assets"
               , postHtmlDir = html </> "posts"
               , imageDir = html </> "generated-images"
               , templateDir = base </> "templates"
               , htmlTempDir = base </> "tmp"
               , baseUrl = cfg_baseUrl
               , title = cfg_title
               , authorName = cfg_authorName
               , authorEmail = cfg_authorEmail
               , configPath = configFile
               , blogPosts = allPosts
               , processors = procs
               , postIndexMTime = indexMod
               , configMTime = configMod
               , baselineMTime = baseline
               , templateMTime = tmplTime
               }

  ensureDirs b
  return b

-- For each configured document processor, run its check routine in
-- case it needs to install data files or do validation.
runProcessorChecks :: BlogM ()
runProcessorChecks = do
  ps <- processors <$> theBlog
  let checks = catMaybes $ checkDataDir <$> ps
  sequence_ checks

doInstallAssets :: BlogM ()
doInstallAssets = do
  ps <- processors <$> theBlog
  let fs = catMaybes $ installAssets <$> ps
  sequence_ fs

regenerateContent :: BlogM ()
regenerateContent = do
  blog <- theBlog

  runProcessorChecks

  generatePosts
  buildIndexPage
  generatePostList

  withTemplate (Files.rssTemplatePath blog) $ \t ->
      liftIO $ writeFile (Files.rssXml blog) $ generateRssFeed blog t

  liftIO $ writeFile (Files.postIndex blog) $
            serializePostIndex $ blogPosts blog

  doInstallAssets

printHandler :: GenEvent -> IO ()
printHandler (PostRender p cs) =
    let cause Config = "config changed"
        cause PostIndex = "post-index changed"
        cause Template = "template changed"
        cause PostModified = "post was modified"
        cause Forced = "forced rebuild"
        reasons = intercalate ", " (cause <$> cs)
    in if cs == [PostModified]
       then putStrLn $ "Rendering post: " ++ (show $ postBaseName p)
       else putStrLn $ "Rendering post (" ++ reasons
                ++ "): " ++ (show $ postBaseName p)
printHandler Finished =
    putStrLn "Done."

main :: IO ()
main = do
  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  newConf <- case initDataDirectory conf of
               True -> do
                 initializeDataDir dir
                 return $ conf { forceRegeneration = True }
               False -> return conf

  case listenMode newConf of
    False -> doGeneration newConf printHandler
    True -> do
         -- This will abort with an error if the blog directory isn't
         -- configured.  Messy.
         _ <- mkBlog newConf

         putStrLn $ "Waiting for changes in " ++ (dataDirectory newConf) ++ " ..."
         scanForChanges (newConf { forceRegeneration = False }) printHandler
