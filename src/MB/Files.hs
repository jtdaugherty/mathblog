module MB.Files where

import System.FilePath
    ( (</>)
    , takeBaseName
    , takeFileName
    )
import MB.Types
    ( Config(..)
    , Post(..)
    )

indexHtml :: Config -> FilePath
indexHtml c = htmlDir c </> "index.html"

rssXml :: Config -> FilePath
rssXml c = htmlDir c </> "rss.xml"

rssPreamble :: Config -> FilePath
rssPreamble c = templateDir c </> "rssPreamble.xml"

rssPostamble :: Config -> FilePath
rssPostamble c = templateDir c </> "rssPostamble.xml"

listHtml :: Config -> FilePath
listHtml c = postHtmlDir c </> "index.html"

listHtex :: Config -> FilePath
listHtex c = htmlTempDir c </> "list.htex"

listTmpHtml :: Config -> FilePath
listTmpHtml c = htmlTempDir c </> "list.html"

firstPost :: Config -> FilePath
firstPost c = postSourceDir c </> "first-post.txt"

pagePreamble :: Config -> FilePath
pagePreamble c = templateDir c </> "pagePreamble.html"

pagePostamble :: Config -> FilePath
pagePostamble c = templateDir c </> "pagePostamble.html"

postPreamble :: Config -> FilePath
postPreamble c = templateDir c </> "postPreamble.html"

postPostamble :: Config -> FilePath
postPostamble c = templateDir c </> "postPostamble.html"

postUrl :: Post -> String
postUrl p = "/posts/" ++ postBaseName p ++ ".html"

postBaseName :: Post -> String
postBaseName = takeBaseName . takeFileName . postFilename

postHtex :: Config -> Post -> String
postHtex config p = htmlTempDir config </> postBaseName p ++ ".htex"

postIntermediateHtml :: Config -> Post -> FilePath
postIntermediateHtml config post =
    postIntermediateDir config </> postBaseName post ++ ".html"

postFinalHtml :: Config -> Post -> FilePath
postFinalHtml config p = postHtmlDir config </> postBaseName p ++ ".html"

stylesheet :: Config -> FilePath
stylesheet c = stylesheetDir c </> "stylesheet.css"
