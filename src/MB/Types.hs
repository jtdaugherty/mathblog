module MB.Types
    ( Blog(..)
    , Post(..)
    , Template
    , ChangeSummary(..)
    , Page(..)
    , Processor(..)
    , nullProcessor
    , noChanges
    )
where
import Data.Time.Clock
    ( UTCTime
    )
import Data.Monoid
import Data.List (nub)
import qualified Text.Pandoc as Pandoc
import Text.StringTemplate
    ( StringTemplate
    )

type Template = StringTemplate String

data Blog = Blog { baseDir :: FilePath
                 , postSourceDir :: FilePath
                 , htmlDir :: FilePath
                 , assetDir :: FilePath
                 , postHtmlDir :: FilePath
                 , imageDir :: FilePath
                 , templateDir :: FilePath
                 , htmlTempDir :: FilePath
                 , baseUrl :: String
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
                 , postModificationString :: String
                 , postAst :: Pandoc.Pandoc
                 , postTeXMacros :: String
                 , postAuthors :: [String]
                 , postDate :: Maybe String
                 }
            deriving (Eq)

data Page = BlogPost
          | Index

data Processor =
    Processor { applyWriterOptions :: Maybe (Pandoc.WriterOptions -> Pandoc.WriterOptions)
              , preProcessPost :: Maybe (Blog -> Post -> IO Post)
              , postProcessPost :: Maybe (Blog -> FilePath -> Page -> IO ())
              , pageHead :: Maybe String
              , buildPostTitle :: Maybe (Page -> [Pandoc.Inline] -> [Pandoc.Inline])
              , rawPostTitle :: Maybe ([Pandoc.Inline] -> String)
              , getChangeSummary :: Maybe (Blog -> UTCTime -> IO ChangeSummary)
              , checkDataDir :: Maybe (Blog -> IO ())
              , installAssets :: Maybe (Blog -> IO ())
              }

nullProcessor :: Processor
nullProcessor =
    Processor Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- Summarize changes in files so we know what to do during the
-- regeneration phase.  postsChanged and configChanged are the primary
-- measurements, but knowing whether other metadata files changed
-- (like templates) is useful for the "listen" mode of mb.
data ChangeSummary =
    ChangeSummary { postsChanged :: [String]
                  , configChanged :: Bool
                  , templatesChanged :: Bool
                  , postIndexChanged :: Bool
                  , assetsChanged :: Bool
                  }
    deriving (Show)

noChanges :: ChangeSummary
noChanges = ChangeSummary [] False False False False

instance Monoid ChangeSummary where
    mempty = noChanges
    a `mappend` b =
        ChangeSummary { postsChanged = nub $ postsChanged a ++ postsChanged b
                      , configChanged = configChanged a || configChanged b
                      , templatesChanged = templatesChanged a || templatesChanged b
                      , postIndexChanged = postIndexChanged a || postIndexChanged b
                      , assetsChanged = assetsChanged a || assetsChanged b
                      }