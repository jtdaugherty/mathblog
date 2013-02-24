module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock
import Data.List
import System.IO
import System.Exit
import System.Directory
    hiding (getModificationTime)
import System.FilePath

import System.FSNotify
import qualified Filesystem.Path.CurrentOS as FP

import qualified MB.Config as Config
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

        isPostIndex f = (dataDirectory conf </> "posts/posts-index") == f

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

getIFSState :: BlogInputFS -> BlogOutputFS -> IO BlogInputFSState
getIFSState ifs ofs =
    BlogInputFSState <$> indexMod <*> configMod <*> baseline <*> template
        where
          fallback = getCurrentTime

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

mkBlog :: StartupConfig -> IO Blog
mkBlog conf = do
  let ifs = blogInputFS conf
      ofs = blogOutputFS conf
      configPath = ifsConfigPath ifs

  e <- doesFileExist configPath
  when (not e) $ do
                  putStrLn $ "Configuration file " ++ configPath ++ " not found"
                  exitFailure

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
  allPosts <- loadPostIndex $ ifsPostSourceDir ifs

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

  -- Get modification times.
  ifsState <- getIFSState ifs ofs

  ensureDirs ifs ofs

  return $ Blog { inputFS = ifs
                , outputFS = ofs
                , inputFSState = ifsState
                , baseUrl = cfg_baseUrl
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
  hSetBuffering stdout NoBuffering

  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  newConf <- case initDataDirectory conf of
               True -> do
                 initializeDataDir dir
                 return $ conf { forceRegeneration = True }
               False -> return conf

  canonicalConfig <- canonicalizeStartupConfig newConf

  case listenMode canonicalConfig of
    False -> doGeneration canonicalConfig printHandler
    True -> do
         -- This will abort with an error if the blog directory isn't
         -- configured.  Messy.
         _ <- mkBlog canonicalConfig

         putStrLn $ "Waiting for changes in " ++ (dataDirectory canonicalConfig) ++ " ..."
         scanForChanges (canonicalConfig { forceRegeneration = False }) printHandler
