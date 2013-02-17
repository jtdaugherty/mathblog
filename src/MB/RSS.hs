module MB.RSS
    ( generateRssFeed
    )
where

import System.IO
import Control.Applicative ((<|>))
import Data.Time.Format
    ( formatTime
    , parseTime
    )
import System.Exit
import System.Locale
    ( rfc822DateFormat
    , defaultTimeLocale
    )
import MB.Types
import MB.Processing ( getRawPostTitle )
import MB.Templates
import qualified MB.Files as Files

rssItem :: Blog -> Post -> String
rssItem blog p =
    concat [ "<item>"
           , "<title>" ++ getRawPostTitle blog p ++ "</title>\n"
           , "<link>" ++ baseUrl blog ++ Files.postUrl p ++ "</link>\n"
           , "<pubDate>" ++ rssModificationTime p ++ "</pubDate>\n"
           , "<guid>" ++ baseUrl blog ++ Files.postUrl p ++ "</guid>\n"
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

rssModificationTime :: Post -> String
rssModificationTime p =
    let Just t = parsed <|> (Just $ postModificationTime p)
        parsed = parseTime defaultTimeLocale "%B %e, %Y" =<< postDate p
    in formatTime defaultTimeLocale rfc822DateFormat t
