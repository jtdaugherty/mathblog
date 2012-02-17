module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe
import System.IO
import System.Exit
import System.Directory
import System.FilePath
import System.Posix.Files
    ( createSymbolicLink
    )
import Data.Time.LocalTime
import qualified Text.Pandoc as Pandoc
import qualified MB.Config as Config
import MB.Templates
    ( loadTemplate
    , renderTemplate
    )
import MB.Processing
import MB.Util
    ( copyTree
    , toLocalTime
    , rssModificationTime
    , loadPostIndex
    , anyChanges
    , serializePostIndex
    , summarizeChanges
    )
import MB.Types
import qualified MB.Files as Files
import Paths_mathblog
    ( getDataFileName
    )
import MB.Startup
    ( dataDirectory
    , listenMode
    , startupConfigFromEnv
    , initDataDirectory
    )
import MB.Gnuplot
import MB.Mathjax
import MB.Gladtex

skelDir :: IO FilePath
skelDir = getDataFileName "skel"

configFilename :: String
configFilename = "blog.cfg"

commonTemplateAttrs :: Blog -> [(String, String)]
commonTemplateAttrs blog =
    [ ( "baseUrl", baseUrl blog )
    , ( "title", title blog )
    , ( "authorName", authorName blog )
    , ( "authorEmail", authorEmail blog )
    , ( "extraPageHead", extraPageHead blog )
    ]

extraPageHead :: Blog -> String
extraPageHead b = concat $ catMaybes $ pageHead <$> processors b

fillTemplate :: Blog -> Template -> [(String, String)] -> String
fillTemplate blog t attrs = renderTemplate attrs' t
    where attrs' = commonTemplateAttrs blog ++ attrs

writeTemplate :: Blog -> Handle -> Template -> [(String, String)] -> IO ()
writeTemplate blog h t attrs = hPutStr h $ fillTemplate blog t attrs

writePost :: Blog -> Handle -> Post -> IO ()
writePost blog h post = do
  let writerOpts = getWriterOptions blog Pandoc.defaultWriterOptions
  created <- postModificationString post
  hPutStr h $ "<h1>" ++ getPostTitle blog post BlogPost ++ "</h1>"
  hPutStr h $ "<span class=\"post-created\">Posted " ++ created ++ "</span>"
  hPutStr h $ Pandoc.writeHtmlString writerOpts (postAst post)

buildLinks :: Blog -> Maybe Post -> Maybe Post -> String
buildLinks blog prev next =
    "<div id=\"prev-next-links\">"
      ++ link "next-link" "older \\(\\Rightarrow\\)" next
      ++ link "prev-link" "\\(\\Leftarrow\\) newer" prev
      ++ "</div>"
        where
          link cls name Nothing =
              "<span class=\"" ++ cls ++ "-subdued\">" ++ name ++ "</span>"
          link cls name (Just p) =
              "<a class=\"" ++ cls ++ "\" href=\"" ++ Files.postUrl blog p ++
                                "\">" ++ name ++ "</a>"

jsInfo :: Post -> String
jsInfo post =
    "<script type=\"text/javascript\">\n" ++
    "Blog = {\n" ++
    "  pageName: " ++ show (Files.postBaseName post) ++
    "\n" ++
    "};\n" ++
    "</script>\n"

buildPage :: Handle -> Blog -> String -> Maybe String -> IO ()
buildPage h blog content extraTitle = do
  eTmpl <- loadTemplate $ Files.pageTemplatePath blog

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          let attrs = [ ("content", content)
                      ] ++ maybe [] (\t -> [("extraTitle", t)]) extraTitle

          writeTemplate blog h tmpl attrs
          hClose h

buildPost :: Handle -> Blog -> Post -> (Maybe Post, Maybe Post) -> IO ()
buildPost h blog post prevNext = do
  eTmpl <- loadTemplate $ Files.postTemplatePath blog

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          html <- readFile $ Files.postIntermediateHtml blog post

          let attrs = [ ("post", html)
                      , ("nextPrevLinks", uncurry (buildLinks blog) prevNext)
                      , ("jsInfo", jsInfo post)
                      ]

          let out = (fillTemplate blog tmpl attrs)
          buildPage h blog out $ Just $ getRawPostTitle blog post

generatePost :: Blog -> Post -> ChangeSummary -> IO ()
generatePost blog post summary = do
  let finalHtml = Files.postIntermediateHtml blog post
      generate = (postFilename post `elem` (postsChanged summary))
                 || configChanged summary

  when generate $ do
    putStrLn $ "Rendering " ++ Files.postBaseName post

    h <- openFile finalHtml WriteMode
    writePost blog h =<< applyPreProcessors blog post
    hClose h
    applyPostProcessors blog finalHtml

generatePosts :: Blog -> ChangeSummary -> IO ()
generatePosts blog summary = do
  let numRegenerated = if configChanged summary
                       then length $ blogPosts blog
                       else length $ postsChanged summary
  when (numRegenerated > 0) $ putStrLn $ "Rendering " ++ (show numRegenerated) ++ " post(s)..."

  let n = length posts
      posts = blogPosts blog
  forM_ (zip posts [0..]) $ \(p, i) ->
      do
        let prevPost = if i == 0 then Nothing else Just (posts !! (i - 1))
            nextPost = if i == n - 1 then Nothing else Just (posts !! (i + 1))

        generatePost blog p summary
        h <- openFile (Files.postFinalHtml blog p) WriteMode
        buildPost h blog p (prevPost, nextPost)
        hClose h

linkIndexPage :: Blog -> IO ()
linkIndexPage blog = do
  let dest = Files.postFinalHtml blog post
      index = Files.indexHtml blog
      post = head $ blogPosts blog

  exists <- doesFileExist index
  when exists $ removeFile index

  createSymbolicLink dest index

postModificationString :: Post -> IO String
postModificationString p = do
  tz <- getCurrentTimeZone
  localTime <- toLocalTime $ postModificationTime p
  return $ show localTime ++ "  " ++ timeZoneName tz

generatePostList :: Blog -> IO ()
generatePostList blog = do
  -- For each post in the order they were given, extract the
  -- unrendered title and construct an htex document.  Then render it
  -- to the listing location.
  entries <- forM (blogPosts blog) $ \p ->
             do
               created <- postModificationString p
               return $ concat [ "<div class=\"listing-entry\"><span class=\"post-title\">"
                               , "<a href=\"" ++ Files.postUrl blog p ++ "\">"
                               , getPostTitle blog p Index
                               , "</a></span><span class=\"post-created\">Posted "
                               , created
                               , "</span></div>\n"
                               ]

  let content = "<div id=\"all-posts\">" ++ concat entries ++ "</div>"

  h <- openFile (Files.listHtml blog) WriteMode
  buildPage h blog content Nothing
  hClose h

rssItem :: Blog -> Post -> String
rssItem blog p =
    concat [ "<item>"
           , "<title>" ++ getRawPostTitle blog p ++ "</title>\n"
           , "<link>" ++ Files.postUrl blog p ++ "</link>\n"
           , "<pubDate>" ++ rssModificationTime p ++ "</pubDate>\n"
           , "<guid>" ++ Files.postUrl blog p ++ "</guid>\n"
           , "</item>\n"
           ]

generateRssFeed :: Blog -> IO ()
generateRssFeed blog = do
  h <- openFile (Files.rssXml blog) WriteMode

  eTmpl <- loadTemplate $ Files.rssTemplatePath blog

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          let items = map (rssItem blog) $ blogPosts blog
              itemStr = concat items
              attrs = [ ("items", itemStr)
                      ]

          writeTemplate blog h tmpl attrs
          hClose h

initializeDataDir :: FilePath -> IO ()
initializeDataDir dir = do
  existsBase <- doesDirectoryExist dir
  existsConfig <- doesFileExist $ dir </> configFilename

  dataDir <- skelDir

  when (not existsBase || not existsConfig) $ do
          putStrLn $ "Setting up data directory using skeleton: " ++ dataDir
          when (not existsBase) $ createDirectory dir
          copyTree dataDir dir

  when existsConfig $
       putStrLn $ "Data directory already initialized; found " ++
                    (dir </> configFilename)

ensureDirs :: Blog -> IO ()
ensureDirs blog = do
  let dirs = [ postSourceDir
             , htmlDir
             , stylesheetDir
             , postHtmlDir
             , postIntermediateDir
             , imageDir
             , templateDir
             , htmlTempDir
             , eqPreamblesDir
             ]

  forM_ (dirs <*> pure blog) $ \d ->
      do
        exists <- doesDirectoryExist d
        when (not exists) $ createDirectory d

scanForChanges :: FilePath -> (FilePath -> IO Bool) -> IO ()
scanForChanges dir act = do
  scan
      where
        scan = do
          didWork <- act dir
          when didWork $ putStrLn ""
          threadDelay $ 1 * 1000 * 1000
          scan

mathBackends :: [(String, Processor)]
mathBackends =
    [ ("gladtex", gladtexProcessor)
    , ("mathjax", mathjaxProcessor)
    ]

mkBlog :: FilePath -> IO Blog
mkBlog base = do
  let configFilePath = base </> configFilename
  e <- doesFileExist configFilePath

  when (not e) $ do
                  putStrLn $ "Configuration file " ++ configFilePath ++ " not found"
                  exitFailure

  let requiredValues = [ "baseUrl"
                       , "title"
                       , "authorName"
                       , "authorEmail"
                       ]

  cfg <- Config.readConfig configFilePath requiredValues

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

      procs = [gnuplotProcessor, mathBackend]

  let b = Blog { baseDir = base
               , postSourceDir = postSrcDir
               , htmlDir = base </> "html"
               , stylesheetDir = base </> "html" </> "stylesheets"
               , postHtmlDir = base </> "html" </> "posts"
               , postIntermediateDir = base </> "generated"
               , imageDir = base </> "html" </> "generated-images"
               , templateDir = base </> "templates"
               , htmlTempDir = base </> "tmp"
               , baseUrl = cfg_baseUrl
               , title = cfg_title
               , authorName = cfg_authorName
               , authorEmail = cfg_authorEmail
               , eqPreamblesDir = base </> "eq-preambles"
               , configPath = configFilePath
               , blogPosts = allPosts
               , processors = procs
               }

  ensureDirs b
  return b

regenerateContent :: FilePath -> IO Bool
regenerateContent dir = do
  blog <- mkBlog dir
  summary <- summarizeChanges blog

  case anyChanges summary of
    False -> return False
    True -> do
      putStrLn $ "Blog directory: " ++ baseDir blog

      when (configChanged summary) $
           putStrLn "Configuration file changed; regenerating all content."
      when (templatesChanged summary) $
           putStrLn "Templates changed; regenerating accordingly."
      when (not $ null $ postsChanged summary) $
           do
             putStrLn "Posts changed:"
             forM_ (postsChanged summary) $ \n -> putStrLn $ "  " ++ n
      when (postIndexChanged summary) $
           putStrLn "Post index changed; regenerating next/previous links."

      generatePosts blog summary

      linkIndexPage blog
      generatePostList blog
      generateRssFeed blog

      writeFile (Files.postIndex blog) $
                serializePostIndex $ blogPosts blog

      putStrLn "Done."
      return True

main :: IO ()
main = do
  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  when (initDataDirectory conf) $ initializeDataDir dir

  case listenMode conf of
    False -> do
         didWork <- regenerateContent dir
         when (not didWork) $ putStrLn "No changes found!"
    True -> scanForChanges dir regenerateContent
