module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Monad.Trans.Either
import Data.Maybe
import Data.Time.Clock
import Data.Time.Calendar
import Data.List
import System.IO
import System.Exit
import System.Directory
    hiding (getModificationTime)
import System.FilePath

import System.FSNotify
import qualified Filesystem.Path.CurrentOS as FP

import qualified MB.Config as Config
import MB.Server
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

ensureDirs :: BlogInputFS -> BlogOutputFS -> IO ()
ensureDirs ifs ofs = do
  let ifsDirs = [ ifsPostSourceDir
                , ifsAssetDir
                , ifsTemplateDir
                ]
      ofsDirs = [ ofsBaseDir
                , ofsPostHtmlDir
                , ofsImageDir
                , ofsHtmlTempDir
                ]

  forM_ (ifsDirs <*> pure ifs) ensureDir
  forM_ (ofsDirs <*> pure ofs) ensureDir

doGeneration :: StartupConfig -> Blog -> (GenEvent -> IO ()) -> IO ()
doGeneration config blog handler = do
  ch <- newChan
  mv <- newEmptyMVar

  let waitForEvents = do
        ev <- readChan ch
        handler ev
        case ev of
          Finished -> putMVar mv ()
          _ -> waitForEvents

  _ <- forkIO waitForEvents
  runBlogM blog ch config regenerateContent

  -- Signal event handler to shut down
  writeChan ch Finished

  -- Wait for event handler to shut down
  _ <- readMVar mv
  return ()

isEventInteresting :: BlogInputFS -> Event -> Bool
isEventInteresting ifs ev =
    let fp = case ev of
               Added f _ -> f
               Modified f _ -> f
               Removed f _ -> f

        isPost f = (ifsPostSourceDir ifs) `isPrefixOf` f &&
                   ".txt" `isSuffixOf` f &&
                   not ("." `isPrefixOf` takeFileName f)

        isPostIndex f = ifsPostIndexPath ifs == f

        isAsset f = (ifsAssetDir ifs) `isPrefixOf` f

        isTemplate f = (ifsTemplateDir ifs) `isPrefixOf` f &&
                       (not ("." `isPrefixOf` takeFileName f)) &&
                       ((".html" `isSuffixOf` f) ||
                        (".xml" `isSuffixOf` f))

        isConfig f = ifsConfigPath ifs == f

    in or ([ isPost
           , isAsset
           , isTemplate
           , isConfig
           , isPostIndex
           ] <*> pure (FP.encodeString fp))

scanForChanges :: StartupConfig
               -> (GenEvent -> IO ())
               -> (Blog -> Blog)
               -> IO ()
               -> IO ()
scanForChanges conf h blogTrans signalAct = do
  let ifs = blogInputFS conf
  forever $ do
    withManager $ \m ->
        do
          let loadBlog =
                  do
                    result <- runEitherT (mkBlog conf)
                    case result of
                      Left e -> do
                          putStrLn $ "Error reading blog configuration: " ++ e
                          threadDelay 1000000
                          loadBlog
                      Right blog -> return $ blogTrans blog

          blog <- loadBlog
          doGeneration conf blog h
          signalAct
          putStrLn ""

          -- Wait for a bit so the filesystem scan doesn't pick up the
          -- posts-index change in the source tree
          threadDelay $ 500 * 1000

          ch <- newChan
          watchTreeChan m (FP.decodeString $ dataDirectory conf) (const True) ch
          let nextEv = do
                ev <- readChan ch
                if isEventInteresting ifs ev then return ev else nextEv
          evt <- nextEv
          case evt of
            Added fp _ -> putStrLn $ "File created: " ++ FP.encodeString fp
            Modified fp _ -> putStrLn $ "File modified: " ++ FP.encodeString fp
            Removed fp _ -> putStrLn $ "File removed: " ++ FP.encodeString fp

mathBackends :: [(String, Processor)]
mathBackends =
    [ ("mathjax", mathjaxProcessor)
    ]

eqBackends :: [(String, Processor)]
eqBackends =
    [ ("tikz", tikzProcessor)
    ]

getIFSState :: BlogInputFS -> BlogOutputFS -> IO BlogInputFSState
getIFSState ifs ofs =
    BlogInputFSState <$> indexMod <*> configMod <*> baseline <*> template
        where
          -- As a fallback timestamp, use a time in the past to ensure
          -- things get regenerated if they're missing.
          fallback = previousDay <$> getCurrentTime
          previousDay t =
              t { utctDay = addDays (-1000) (utctDay t) }

          indexMod = do
            let pth = ifsPostIndexPath ifs
            ex <- doesFileExist pth
            if ex then getModificationTime pth else fallback

          configMod = do
            let pth = ifsConfigPath ifs
            ex <- doesFileExist pth
            if ex then getModificationTime pth else fallback

          template = do
            contents <- getDirectoryContents $ ifsTemplateDir ifs
            let templates = filter (not . flip elem [".", ".."]) contents
                templatePath = (ifsTemplateDir ifs </>)
            last <$> sort <$>
                 mapM getModificationTime (templatePath <$> templates)

          baseline = do
            let indexHtml = ofsIndexHtml ofs
            ex <- doesFileExist indexHtml
            if ex then getModificationTime indexHtml else fallback

mkBlog :: StartupConfig -> EitherT String IO Blog
mkBlog conf = do
  let ifs = blogInputFS conf
      ofs = blogOutputFS conf
      configPath = ifsConfigPath ifs

  e <- liftIO $ doesFileExist configPath
  case e of
    False -> left $ "Configuration file " ++ configPath ++ " not found"
    True -> return ()

  let requiredValues = [ "baseUrl"
                       , "title"
                       , "authorName"
                       , "authorEmail"
                       ]

  cfg <- Config.readConfig configPath requiredValues

  let Just cfg_baseUrl = lookup "baseUrl" cfg
      Just cfg_title = lookup "title" cfg
      Just cfg_authorName = lookup "authorName" cfg
      Just cfg_authorEmail = lookup "authorEmail" cfg

  -- Load blog posts from disk
  allPosts <- liftIO $ loadPostIndex ifs

  let requestedMathBackend = lookup "mathBackend" cfg
      isBackendRequested (nam, p) =
          let Just opt = lookup nam cfg <|> Just "no"
          in if Config.affirmative opt
             then Just p
             else Nothing

  mathBackend <- case requestedMathBackend of
                   Nothing -> return mathjaxProcessor
                   Just b -> case lookup b mathBackends of
                               Nothing -> left $ "Unsupported math backend " ++ show b
                                          ++ "; valid choices are "
                                          ++ (show $ fst <$> mathBackends)
                               Just proc -> return proc

  let procs = baseProcessor : eqBackendConfig ++ [mathBackend]
      eqBackendConfig = catMaybes $ isBackendRequested <$> eqBackends

  ifsState <- liftIO $ getIFSState ifs ofs
  liftIO $ ensureDirs ifs ofs

  return $ Blog { inputFS = ifs
                , outputFS = ofs
                , inputFSState = ifsState
                , baseUrl = fromJust (overrideBaseUrl conf <|> Just cfg_baseUrl)
                , title = cfg_title
                , authorName = cfg_authorName
                , authorEmail = cfg_authorEmail
                , blogPosts = allPosts
                , processors = procs
                }

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
  let ifs = inputFS blog
      ofs = outputFS blog

  runProcessorChecks

  generatePosts
  buildIndexPage
  generatePostList

  withTemplate (ifsRssTemplatePath ifs) $ \t ->
      liftIO $ writeFile (ofsRssXml ofs) $ generateRssFeed blog t

  liftIO $ writeFile (ifsPostIndexPath ifs) $
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
  -- This is only to make it possible to run "mb" during the LaTeX
  -- manual build process.  I run "mb" to generate output for
  -- demonstrations in the manual and output buffering was causing
  -- output to not get sent to pdfLaTeX.
  hSetBuffering stdout NoBuffering

  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  newConf <- case initDataDirectory conf of
               True -> do
                 initializeDataDir dir
                 return $ conf { forceRegeneration = True }
               False -> return conf

  canonicalConfig <- canonicalizeStartupConfig newConf

  result <- runEitherT (mkBlog canonicalConfig)
  blog <- case result of
            Left e -> (putStrLn $ "Error: " ++ e) >> exitFailure
            Right b -> return b

  case listenAddr canonicalConfig of
    Nothing -> doGeneration canonicalConfig blog printHandler
    Just _ -> do
         putStrLn $ "Starting up in listen mode..."
         let conf' = canonicalConfig { forceRegeneration = False }
         withServing conf' $ flip scanForChanges printHandler
