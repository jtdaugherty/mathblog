module MB.Files where

import System.FilePath
    ( (</>)
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

rssTemplatePath :: Blog -> FilePath
rssTemplatePath c = templateDir c </> "rssTemplate.xml"

listHtml :: Blog -> FilePath
listHtml c = postHtmlDir c </> "index.html"

postIndex :: Blog -> FilePath
postIndex c = postSourceDir c </> "posts-index"

pageTemplatePath :: Blog -> FilePath
pageTemplatePath c = templateDir c </> "pageTemplate.html"

listTemplatePath :: Blog -> FilePath
listTemplatePath c = templateDir c </> "listTemplate.html"

postTemplatePath :: Blog -> FilePath
postTemplatePath c = templateDir c </> "postTemplate.html"

postFinalHtml :: Blog -> Post -> FilePath
postFinalHtml config p = postHtmlDir config </> postHtmlFilename p
