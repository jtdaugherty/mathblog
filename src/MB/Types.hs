{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MB.Types
    ( Config(..)
    , Post(..)
    , PostIndex(..)
    , Template
    )
where
import Data.Time.Clock
    ( UTCTime
    )
import qualified Text.Pandoc.Definition as Pandoc
import Text.StringTemplate
    ( StringTemplate
    )

type Template = StringTemplate String

data Config = Config { baseDir :: FilePath
                     , postSourceDir :: FilePath
                     , htmlDir :: FilePath
                     , stylesheetDir :: FilePath
                     , postHtmlDir :: FilePath
                     , postIntermediateDir :: FilePath
                     , imageDir :: FilePath
                     , templateDir :: FilePath
                     , htmlTempDir :: FilePath
                     , baseUrl :: String
                     , eqPreamblesDir :: FilePath
                     , title :: String
                     , authorName :: String
                     , authorEmail :: String
                     }

data Post = Post { postTitle :: Int -> String
                 , postTitleRaw :: String
                 , postPath :: String
                 , postFilename :: String
                 , postModificationTime :: UTCTime
                 , postAst :: Pandoc.Pandoc
                 }

newtype PostIndex = PostIndex [Post]
