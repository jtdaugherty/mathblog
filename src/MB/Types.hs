module MB.Types
    ( Blog(..)
    , StartupConfig(..)
    , BlogM
    , GenEvent(..)
    , GenState(..)
    , Post(..)
    , Template
    , ChangeSummary(..)
    , Page(..)
    , Processor(..)
    , RenderCause(..)
    , nullProcessor
    , noChanges
    , theBlog
    , theConfig
    , notify
    , runBlogM
    )
where
import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.Time.Clock
    ( UTCTime
    )
import Data.Monoid
import Data.List (nub)
import qualified Text.Pandoc as Pandoc
import Text.StringTemplate
    ( StringTemplate
    )

data StartupConfig = StartupConfig { listenMode :: Bool
                                   , dataDirectory :: FilePath
                                   , initDataDirectory :: Bool
                                   , forceRegeneration :: Bool
                                   , htmlOutputDirectory :: FilePath
                                   , configFilePath :: FilePath
                                   }
                     deriving (Show, Eq)

data GenState =
    GenState { stBlog :: Blog
             , stChan :: Chan GenEvent
             , stConfig :: StartupConfig
             }

data GenEvent = PostRender Post [RenderCause]
              | Finished

data RenderCause = Config
                 | PostIndex
                 | Template
                 | PostModified
                 | Forced
                   deriving (Eq, Show)

type BlogM a = ReaderT GenState IO a

theBlog :: BlogM Blog
theBlog = asks stBlog

theConfig :: BlogM StartupConfig
theConfig = asks stConfig

notify :: GenEvent -> BlogM ()
notify ev = do
  ch <- asks stChan
  liftIO $ writeChan ch ev

runBlogM :: Blog -> Chan GenEvent -> StartupConfig -> BlogM a -> IO a
runBlogM b ch conf act = runReaderT act (GenState b ch conf)

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
                 , postIndexMTime :: UTCTime
                 , configMTime :: UTCTime
                 , templateMTime :: UTCTime
                 , baselineMTime :: UTCTime
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
                 , postBaseName :: String
                 , postUrl :: String
                 , postHtmlFilename :: String
                 }
            deriving (Eq)

data Page = BlogPost
          | Index

data Processor =
    Processor { applyWriterOptions :: Maybe (Pandoc.WriterOptions -> Pandoc.WriterOptions)
              , preProcessPost :: Maybe (Post -> BlogM Post)
              , postProcessPost :: Maybe (FilePath -> Page -> BlogM ())
              , pageHead :: Maybe String
              , buildPostTitle :: Maybe (Page -> [Pandoc.Inline] -> [Pandoc.Inline])
              , rawPostTitle :: Maybe ([Pandoc.Inline] -> String)
              , checkDataDir :: Maybe (BlogM ())
              , installAssets :: Maybe (BlogM ())
              }

nullProcessor :: Processor
nullProcessor =
    Processor Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- Summarize changes in files so we know what to do during the
-- regeneration phase.  postsChanged and configChanged are the primary
-- measurements, but knowing whether other metadata files changed
-- (like templates) is useful for the "listen" mode of mb.
data ChangeSummary =
    ChangeSummary { postsChanged :: [String]
                  , configChanged :: Bool
                  , templatesChanged :: [FilePath]
                  , postIndexChanged :: Bool
                  , assetsChanged :: [FilePath]
                  }
    deriving (Show)

noChanges :: ChangeSummary
noChanges = ChangeSummary [] False [] False []

instance Monoid ChangeSummary where
    mempty = noChanges
    a `mappend` b =
        ChangeSummary { postsChanged = nub $ postsChanged a ++ postsChanged b
                      , configChanged = configChanged a || configChanged b
                      , templatesChanged = templatesChanged a `mappend` templatesChanged b
                      , postIndexChanged = postIndexChanged a || postIndexChanged b
                      , assetsChanged = assetsChanged a `mappend` assetsChanged b
                      }