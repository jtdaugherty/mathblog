module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.Monoid
import System.Exit
import System.Directory
import System.FilePath

import System.FSNotify
import qualified Filesystem.Path.CurrentOS as FP

import qualified MB.Config as Config
import qualified MB.Files as Files
import MB.Util
import MB.Changes
import MB.Types
import MB.Startup
import MB.Initialize
import MB.Templates

import MB.Gen.Post
import MB.Gen.PostList
import MB.Gen.Index
import MB.Gen.RSS

import MB.Processors.Gnuplot
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

evFilepath :: Event -> FP.FilePath
evFilepath (Added fp _) = fp
evFilepath (Modified fp _) = fp
evFilepath (Removed fp _) = fp

changesFromEvent :: Blog -> Event -> ChangeSummary
changesFromEvent config ev =
    let fp = evFilepath ev
        fpStr = FP.encodeString fp
        cases = [ ((== (configPath config)), const $ mempty {configChanged = True})
                , (((postSourceDir config) `isPrefixOf`), \f -> mempty {postsChanged = []})
                ]
    in mconcat $ (\c -> if fst c fpStr then snd c else mempty) <$> cases

scanForChanges :: StartupConfig -> IO ()
scanForChanges conf = do
  b <- mkBlog conf
  ch <- newChan
  withManager $ \m ->
      do
        -- watchTree :: WatchManager -> FilePath -> (Event -> Bool) -> (Event -> IO ()) -> IO ()
        -- Event: data Event =
        -- Added    FilePath UTCTime
        -- Modified FilePath UTCTime
        -- Removed  FilePath UTCTime
        watchTreeChan m (FP.decodeString $ dataDirectory conf) (const True) ch

        forever $ do
          ev <- readChan ch
          _ <- regenerateContent conf $ changesFromEvent b ev
          return ()

mathBackends :: [(String, Processor)]
mathBackends =
    [ ("mathjax", mathjaxProcessor)
    ]

-- Note that the order here matters, because the processors will be
-- applied in the order listed.  So if the user has both turned on
-- then gnuplot will try to render a post with the "tikz" preamble,
-- which we don't really want.  So let tikz go first, rewrite the AST,
-- and then let gnuplot handle the rest.
eqBackends :: [(String, Processor)]
eqBackends =
    [ ("tikz", tikzProcessor)
    , ("gnuplot", gnuplotProcessor)
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
      b = Blog { baseDir = base
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
               }

  ensureDirs b
  return b

-- For each configured document processor, run its check routine in
-- case it needs to install data files or do validation.
runProcessorChecks :: Blog -> IO ()
runProcessorChecks blog =
    let checks = catMaybes $ checkDataDir <$> processors blog
    in sequence_ $ checks <*> pure blog

doInstallAssets :: Blog -> IO ()
doInstallAssets blog =
    let fs = catMaybes $ installAssets <$> processors blog
    in sequence_ $ fs <*> pure blog

regenerateContent :: StartupConfig -> ChangeSummary -> IO Bool
regenerateContent conf summary = do
  blog <- mkBlog conf
  runProcessorChecks blog

  case anyChanges summary of
    False -> return False
    True -> do
      putStrLn $ "Blog directory: " ++ baseDir blog
      putStrLn $ "Config file:    " ++ configFilePath conf

      putStrLn "Changes found:"

      when (configChanged summary) $
           putStrLn "  Configuration file"

      when (postIndexChanged summary) $
           putStrLn "  Post index"

      when (not $ null $ postsChanged summary) $
           do
             putStrLn "  Posts:"
             forM_ (postsChanged summary) $ \p ->
                 putStrLn $ "    " ++ p

      when (not $ null $ templatesChanged summary) $
           do
             putStrLn "  Templates:"
             forM_ (templatesChanged summary) $ \t ->
                 putStrLn $ "    " ++ t

      when (not $ null $ assetsChanged summary) $
           do
             putStrLn "  Assets:"
             forM_ (assetsChanged summary) $ \a ->
                 putStrLn $ "    " ++ a

      let posts = [ p | p <- blogPosts blog
                  , or [ postFilename p `elem` postsChanged summary
                       , postIndexChanged summary
                       , configChanged summary
                       , forceRegeneration conf
                       , not $ null $ templatesChanged summary
                       ]
                  ]

      generatePosts blog posts
      buildIndexPage blog
      generatePostList blog

      withTemplate (Files.rssTemplatePath blog) $ \t ->
          writeFile (Files.rssXml blog) $ generateRssFeed blog t

      writeFile (Files.postIndex blog) $
                serializePostIndex $ blogPosts blog

      when (not $ null $ assetsChanged summary) $
           doInstallAssets blog

      putStrLn "Done."
      return True

main :: IO ()
main = do
  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  when (initDataDirectory conf) $ initializeDataDir dir

  case listenMode conf of
    False -> do
         blog <- mkBlog conf
         changes <- summarizeChanges blog (forceRegeneration conf)
         didWork <- regenerateContent conf changes
         when (not didWork) $ putStrLn "No changes found!"
    True -> do
         putStrLn $ "Waiting for changes in " ++ (dataDirectory conf) ++ " ..."
         scanForChanges (conf { forceRegeneration = False })
