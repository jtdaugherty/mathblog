module MB.Gen.RSS
    ( generateRssFeed
    )
where

import Control.Applicative ((<|>))
import Data.Time.Format
    ( formatTime
    , parseTime
    )
import System.Locale
    ( rfc822DateFormat
    , defaultTimeLocale
    )
import MB.Types
import MB.Processing ( getRawPostTitle )
import MB.Templates

rssItem :: Blog -> Post -> String
rssItem blog p =
    concat [ "<item>"
           , "<title>" ++ getRawPostTitle blog p ++ "</title>\n"
           , "<link>" ++ baseUrl blog ++ postUrl p ++ "</link>\n"
           , "<pubDate>" ++ rssModificationTime p ++ "</pubDate>\n"
           , "<guid>" ++ baseUrl blog ++ postUrl p ++ "</guid>\n"
           , "</item>\n"
           ]

generateRssFeed :: Blog -> Template -> String
generateRssFeed blog tmpl =
    let items = map (rssItem blog) $ blogPosts blog
        itemStr = concat items
        attrs = [ ("items", itemStr)
                ]
    in fillTemplate blog tmpl attrs

rssModificationTime :: Post -> String
rssModificationTime p =
    let Just t = parsed <|> (Just $ postModificationTime p)
        parsed = parseTime defaultTimeLocale "%B %e, %Y" =<< postDate p
    in formatTime defaultTimeLocale rfc822DateFormat t
