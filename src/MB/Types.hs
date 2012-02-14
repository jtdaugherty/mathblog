module MB.Types
    ( Blog(..)
    , Post(..)
    , Template
    , ChangeSummary(..)
    , Processor(..)
    )
where
import Data.Time.Clock
    ( UTCTime
    )
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as Pandoc
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

data Post = Post { postTitle :: String
                 , postTitleRaw :: String
                 , postPath :: String
                 , postFilename :: String
                 , postModificationTime :: UTCTime
                 , postAst :: Pandoc.Pandoc
                 }

data Processor =
    Processor { applyWriterOptions :: Pandoc.WriterOptions -> Pandoc.WriterOptions
              , processPost :: Blog -> Post -> IO Post
              }

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