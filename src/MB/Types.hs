module MB.Types
    ( Blog(..)
    , Post(..)
    , Template
    , ChangeSummary(..)
    , TitleSetting(..)
    , Processor(..)
    , nullProcessor
    )
where
import Data.Time.Clock
    ( UTCTime
    )
import qualified Text.Pandoc as Pandoc
import Text.StringTemplate
    ( StringTemplate
    )

type Template = StringTemplate String

data Blog = Blog { baseDir :: FilePath
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
                 , configPath :: FilePath
                 , blogPosts :: [Post]
                 , processors :: [Processor]
                 }

data Post = Post { postTitle :: [Pandoc.Inline]
                 , postPath :: String
                 , postFilename :: String
                 , postModificationTime :: UTCTime
                 , postAst :: Pandoc.Pandoc
                 }

data TitleSetting = BlogPost
                  | Index

data Processor =
    Processor { applyWriterOptions :: Maybe (Pandoc.WriterOptions -> Pandoc.WriterOptions)
              , processPost :: Maybe (Blog -> Post -> IO Post)
              , pageHead :: Maybe String
              , buildPostTitle :: Maybe (TitleSetting -> [Pandoc.Inline] -> [Pandoc.Inline])
              , rawPostTitle :: Maybe ([Pandoc.Inline] -> String)
              }

nullProcessor :: Processor
nullProcessor =
    Processor Nothing Nothing Nothing Nothing Nothing

-- Summarize changes in files so we know what to do during the
-- regeneration phase.  postsChanged and configChanged are the primary
-- measurements, but knowing whether other metadata files changed
-- (like templates) is useful for the "listen" mode of mb.
data ChangeSummary =
    ChangeSummary { postsChanged :: [String]
                  , configChanged :: Bool
                  , templatesChanged :: Bool
                  , postIndexChanged :: Bool
                  }
    deriving (Show)