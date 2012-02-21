module MB.Files where

import System.FilePath
    ( (</>)
    , takeBaseName
    )
import MB.Types
    ( Blog(..)
    , Post(..)
    )

indexHtml :: Blog -> FilePath
indexHtml c = htmlDir c </> "index.html"

imageFilename :: Blog -> String -> FilePath
imageFilename c fn = imageDir c </> fn

rssXml :: Blog -> FilePath
rssXml c = htmlDir c </> "feed.xml"

eqPreambleFile :: Blog -> String -> FilePath
eqPreambleFile config n = eqPreamblesDir config </> n

rssTemplatePath :: Blog -> FilePath
rssTemplatePath c = templateDir c </> "rssTemplate.xml"

listHtml :: Blog -> FilePath
listHtml c = postHtmlDir c </> "index.html"

postIndex :: Blog -> FilePath
postIndex c = postSourceDir c </> "posts-index"

listTmpHtml :: Blog -> FilePath
listTmpHtml c = htmlTempDir c </> "list.html"

firstPost :: Blog -> FilePath
firstPost c = postSourceDir c </> "first-post.txt"

pageTemplatePath :: Blog -> FilePath
pageTemplatePath c = templateDir c </> "pageTemplate.html"

postTemplatePath :: Blog -> FilePath
postTemplatePath c = templateDir c </> "postTemplate.html"

postUrl :: Post -> String
postUrl p = "/posts/" ++ postBaseName p ++ ".html"

postBaseName :: Post -> String
postBaseName = takeBaseName . postFilename

postIntermediateHtml :: Blog -> Post -> FilePath
postIntermediateHtml config post =
    postIntermediateDir config </> postBaseName post ++ ".html"

postFinalHtml :: Blog -> Post -> FilePath
postFinalHtml config p = postHtmlDir config </> postBaseName p ++ ".html"
