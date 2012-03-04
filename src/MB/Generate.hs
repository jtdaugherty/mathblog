module MB.Generate
    ( buildIndexPage
    , generatePostList
    , generatePosts
    )
where

import Control.Monad
import Data.Time.LocalTime
import Data.Time.Clock
import System.Directory
import System.IO
import System.Exit

import qualified Text.Pandoc as Pandoc
import MB.Processing
import MB.Types
import MB.Templates
import qualified MB.Files as Files

writePost :: Blog -> Handle -> Post -> IO ()
writePost blog h post = do
  let writerOpts = getWriterOptions blog Pandoc.defaultWriterOptions
  created <- postModificationString post
  hPutStr h $ "<h1>" ++ getPostTitle blog post BlogPost ++ "</h1>"
  hPutStr h $ "<span class=\"post-created\">Posted " ++ created ++ "</span>"
  hPutStr h $ Pandoc.writeHtmlString writerOpts (postAst post)

buildLinks :: Blog -> Maybe Post -> Maybe Post -> String
buildLinks _blog prev next =
    "<div id=\"prev-next-links\">"
      ++ link "next-link" "older &raquo;" next
      ++ link "prev-link" "&laquo; newer" prev
      ++ "</div>"
        where
          link cls name Nothing =
              "<span class=\"" ++ cls ++ "-subdued\">" ++ name ++ "</span>"
          link cls name (Just p) =
              "<a class=\"" ++ cls ++ "\" href=\"" ++ Files.postUrl p ++
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

    newPost <- applyPreProcessors blog post

    h <- openFile finalHtml WriteMode
    writePost blog h newPost
    hClose h
    applyPostProcessors blog finalHtml BlogPost

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

buildIndexPage :: Blog -> IO ()
buildIndexPage blog = do
  let src = Files.postFinalHtml blog post
      index = Files.indexHtml blog
      post = head $ blogPosts blog

  copyFile src index

postModificationString :: Post -> IO String
postModificationString p = do
  tz <- getCurrentTimeZone
  localTime <- toLocalTime $ postModificationTime p
  return $ show localTime ++ "  " ++ timeZoneName tz

toLocalTime :: UTCTime -> IO LocalTime
toLocalTime u = do
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz u

generatePostList :: Blog -> IO ()
generatePostList blog = do
  -- For each post in the order they were given, extract the
  -- unrendered title and construct an htex document.  Then render it
  -- to the listing location.
  entries <- forM (blogPosts blog) $ \p ->
             do
               created <- postModificationString p
               return $ concat [ "<div class=\"listing-entry\"><span class=\"post-title\">"
                               , "<a href=\"" ++ Files.postUrl p ++ "\">"
                               , getPostTitle blog p Index
                               , "</a></span><span class=\"post-created\">Posted "
                               , created
                               , "</span></div>\n"
                               ]

  let content = "<div id=\"all-posts\">" ++ concat entries ++ "</div>"

  h <- openFile (Files.listHtml blog) WriteMode
  buildPage h blog content Nothing
  hClose h

  applyPostProcessors blog (Files.listHtml blog) Index
