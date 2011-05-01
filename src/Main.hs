module Main where

import Control.Applicative
    ( (<*>)
    , pure
    )
import Control.Monad
    ( when
    , forM_
    , forM
    )
import Control.Concurrent
    ( threadDelay
    )
import System.IO
    ( IOMode(WriteMode)
    , Handle
    , openFile
    , hPutStr
    , hClose
    )
import System.Exit
    ( exitFailure
    )
import System.Directory
    ( doesDirectoryExist
    , doesFileExist
    , removeFile
    , createDirectory
    )
import System.FilePath
    ( (</>)
    )
import System.Posix.Files
    ( createSymbolicLink
    )
import Data.Time.LocalTime
    ( TimeZone(timeZoneName)
    , getCurrentTimeZone
    )
import qualified Text.Pandoc as Pandoc
import qualified MB.Config as Config
import MB.Templates
    ( loadTemplate
    , renderTemplate
    )
import MB.Processing
    ( processPost
    )
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
    )

skelDir :: IO FilePath
skelDir = getDataFileName "skel"

configFilename :: String
configFilename = "blog.cfg"

commonTemplateAttrs :: Blog -> [(String, String)]
commonTemplateAttrs config =
    [ ( "baseUrl", baseUrl config )
    , ( "title", title config )
    , ( "authorName", authorName config )
    , ( "authorEmail", authorEmail config )
    ]

fillTemplate :: Blog -> Template -> [(String, String)] -> String
fillTemplate config t attrs = renderTemplate attrs' t
    where attrs' = commonTemplateAttrs config ++ attrs

writeTemplate :: Blog -> Handle -> Template -> [(String, String)] -> IO ()
writeTemplate config h t attrs = hPutStr h $ fillTemplate config t attrs

pandocWriterOptions :: Pandoc.WriterOptions
pandocWriterOptions =
    Pandoc.defaultWriterOptions { Pandoc.writerHTMLMathMethod = Pandoc.MathJax "MathJax/MathJax.js"
                                }

writePost :: Handle -> Post -> IO ()
writePost h post = do
  created <- postModificationString post
  hPutStr h $ "<h1>" ++ postTitle post ++ "</h1>"
  hPutStr h $ "<span class=\"post-created\">Posted " ++ created ++ "</span>"
  hPutStr h $ Pandoc.writeHtmlString pandocWriterOptions (postAst post)

buildLinks :: Blog -> Maybe Post -> Maybe Post -> String
buildLinks config prev next =
    "<div id=\"prev-next-links\">"
      ++ link "next-link" "older \\(\\Rightarrow\\)" next
      ++ link "prev-link" "\\(\\Leftarrow\\) newer" prev
      ++ "</div>"
        where
          link cls name Nothing =
              "<span class=\"" ++ cls ++ "-subdued\">" ++ name ++ "</span>"
          link cls name (Just p) =
              "<a class=\"" ++ cls ++ "\" href=\"" ++ Files.postUrl config p ++
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
buildPage h config content extraTitle = do
  eTmpl <- loadTemplate $ Files.pageTemplatePath config

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          let attrs = [ ("content", content)
                      ] ++ maybe [] (\t -> [("extraTitle", t)]) extraTitle

          writeTemplate config h tmpl attrs
          hClose h

buildPost :: Handle -> Blog -> Post -> (Maybe Post, Maybe Post) -> IO ()
buildPost h config post prevNext = do
  eTmpl <- loadTemplate $ Files.postTemplatePath config

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          html <- readFile $ Files.postIntermediateHtml config post

          let attrs = [ ("post", html)
                      , ("nextPrevLinks", uncurry (buildLinks config) prevNext)
                      , ("jsInfo", jsInfo post)
                      ]

          let out = (fillTemplate config tmpl attrs)
          buildPage h config out $ Just $ postTitleRaw post

generatePost :: Blog -> Post -> ChangeSummary -> IO ()
generatePost config post summary = do
  let finalHtml = Files.postIntermediateHtml config post
      generate = (postFilename post `elem` (postsChanged summary))
                 || configChanged summary

  when generate $ do
    putStrLn $ "Rendering " ++ Files.postBaseName post

    h <- openFile finalHtml WriteMode
    writePost h =<< processPost config post
    hClose h

generatePosts :: Blog -> ChangeSummary -> IO ()
generatePosts config summary = do
  let numRegenerated = if configChanged summary
                       then length $ blogPosts config
                       else length $ postsChanged summary
  when (numRegenerated > 0) $ putStrLn $ "Rendering " ++ (show numRegenerated) ++ " post(s)..."

  let n = length posts
      posts = blogPosts config
  forM_ (zip posts [0..]) $ \(p, i) ->
      do
        let prevPost = if i == 0 then Nothing else Just (posts !! (i - 1))
            nextPost = if i == n - 1 then Nothing else Just (posts !! (i + 1))

        generatePost config p summary
        h <- openFile (Files.postFinalHtml config p) WriteMode
        buildPost h config p (prevPost, nextPost)
        hClose h

generateIndex :: Blog -> IO ()
generateIndex config = do
  let dest = Files.postFinalHtml config post
      index = Files.indexHtml config
      post = head $ blogPosts config

  exists <- doesFileExist index
  when exists $ removeFile index

  createSymbolicLink dest index

postModificationString :: Post -> IO String
postModificationString p = do
  tz <- getCurrentTimeZone
  localTime <- toLocalTime $ postModificationTime p
  return $ show localTime ++ "  " ++ timeZoneName tz

generateList :: Blog -> IO ()
generateList config = do
  -- For each post in the order they were given, extract the
  -- unrendered title and construct an htex document.  Then render it
  -- to the listing location.
  entries <- forM (blogPosts config) $ \p ->
             do
               created <- postModificationString p
               return $ concat [ "<div class=\"listing-entry\"><span class=\"post-title\">"
                               , "<a href=\"" ++ Files.postUrl config p ++ "\">"
                               , postTitle p
                               , "</a></span><span class=\"post-created\">Posted "
                               , created
                               , "</span></div>\n"
                               ]

  let content = "<div id=\"all-posts\">" ++ concat entries ++ "</div>"

  h <- openFile (Files.listHtml config) WriteMode
  buildPage h config content Nothing
  hClose h

rssItem :: Blog -> Post -> String
rssItem config p =
    concat [ "<item>"
           , "<title>" ++ postTitleRaw p ++ "</title>\n"
           , "<link>" ++ Files.postUrl config p ++ "</link>\n"
           , "<pubDate>" ++ rssModificationTime p ++ "</pubDate>\n"
           , "<guid>" ++ Files.postUrl config p ++ "</guid>\n"
           , "</item>\n"
           ]

generateRssFeed :: Blog -> IO ()
generateRssFeed config = do
  h <- openFile (Files.rssXml config) WriteMode

  eTmpl <- loadTemplate $ Files.rssTemplatePath config

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          let items = map (rssItem config) $ blogPosts config
              itemStr = concat items
              attrs = [ ("items", itemStr)
                      ]

          writeTemplate config h tmpl attrs
          hClose h

setup :: FilePath -> IO ()
setup dir = do
  exists <- doesDirectoryExist dir
  dataDir <- skelDir

  when (not exists) $ do
          putStrLn $ "Setting up data directory using skeleton: " ++ dataDir
          copyTree dataDir dir

ensureDirs :: Blog -> IO ()
ensureDirs config = do
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

  forM_ (dirs <*> pure config) $ \d ->
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

  return $ Blog { baseDir = base
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
                }

regenerateContent :: FilePath -> IO Bool
regenerateContent dir = do
  config <- mkBlog dir
  summary <- summarizeChanges config

  case anyChanges summary of
    True -> do
      putStrLn $ "Blog directory: " ++ baseDir config

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

      generatePosts config summary

      generateIndex config
      generateList config
      generateRssFeed config

      writeFile (Files.postIndex config) $
                serializePostIndex $ blogPosts config

      putStrLn "Done."
      return True
    False -> return False

main :: IO ()
main = do
  conf <- startupConfigFromEnv
  let dir = dataDirectory conf

  setup dir
  blog <- mkBlog dir
  ensureDirs blog

  case listenMode conf of
    False -> do
         didWork <- regenerateContent dir
         when (not didWork) $ putStrLn "No changes found!"
    True -> scanForChanges dir regenerateContent
