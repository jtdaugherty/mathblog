module MB.Files where

import System.FilePath
    ( (</>)
    , takeBaseName
    )
import MB.Types
    ( Config(..)
    , Post(..)
    )

indexHtml :: Config -> FilePath
indexHtml c = htmlDir c </> "index.html"

imageFilename :: Config -> String -> FilePath
imageFilename c fn = imageDir c </> fn

rssXml :: Config -> FilePath
rssXml c = htmlDir c </> "feed.xml"

eqPreambleFile :: Config -> String -> FilePath
eqPreambleFile config n = eqPreamblesDir config </> n

rssTemplatePath :: Config -> FilePath
rssTemplatePath c = templateDir c </> "rssTemplate.xml"

listHtml :: Config -> FilePath
listHtml c = postHtmlDir c </> "index.html"

postIndex :: Config -> FilePath
postIndex c = postSourceDir c </> "posts-index"

listTmpHtml :: Config -> FilePath
listTmpHtml c = htmlTempDir c </> "list.html"

firstPost :: Config -> FilePath
firstPost c = postSourceDir c </> "first-post.txt"

pageTemplatePath :: Config -> FilePath
pageTemplatePath c = templateDir c </> "pageTemplate.html"

postTemplatePath :: Config -> FilePath
postTemplatePath c = templateDir c </> "postTemplate.html"

postUrl :: Config -> Post -> String
postUrl c p = baseUrl c ++ "/posts/" ++ postBaseName p ++ ".html"

postBaseName :: Post -> String
postBaseName = takeBaseName . postFilename

postIntermediateHtml :: Config -> Post -> FilePath
postIntermediateHtml config post =
    postIntermediateDir config </> postBaseName post ++ ".html"

postFinalHtml :: Config -> Post -> FilePath
postFinalHtml config p = postHtmlDir config </> postBaseName p ++ ".html"

stylesheet :: Config -> FilePath
stylesheet c = stylesheetDir c </> "stylesheet.css"
