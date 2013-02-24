module MB.Processing
    ( applyPreProcessors
    , applyPostProcessors
    , getWriterOptions
    , getRawPostTitle
    , getPostTitle
    )
where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import qualified Text.Pandoc as Pandoc
import MB.Types
import MB.Util

applyPreProcessors :: Post -> BlogM Post
applyPreProcessors post = do
  ps <- processors <$> theBlog
  applyPreProcessors_ post ps

applyPostProcessors :: FilePath -> Page -> BlogM ()
applyPostProcessors path pg = do
  ps <- processors <$> theBlog
  applyPostProcessors_ path pg ps

applyPreProcessors_ :: Post -> [Processor] -> BlogM Post
applyPreProcessors_ post [] = return post
applyPreProcessors_ post (p:ps) = do
  post' <- case preProcessPost p of
             Nothing -> return post
             Just f -> f post
  applyPreProcessors_ post' ps

applyPostProcessors_ :: FilePath -> Page -> [Processor] -> BlogM ()
applyPostProcessors_ _ _ [] = return ()
applyPostProcessors_ pth pg (p:ps) = do
  case postProcessPost p of
    Nothing -> return ()
    Just f -> f pth pg
  applyPostProcessors_ pth pg ps

getWriterOptions :: Blog -> Pandoc.WriterOptions -> Pandoc.WriterOptions
getWriterOptions b = foldl (.) id (catMaybes $ applyWriterOptions <$> processors b)

getRawPostTitle :: Blog -> Post -> String
getRawPostTitle b p = head (fs ++ [fallback]) $ postTitle p
    where
      fallback = concat . (getInlineStr <$>)
      fs = catMaybes $ rawPostTitle <$> processors b

getPostTitle :: Blog -> Post -> Page -> String
getPostTitle b p s = concat $ toPostTitle <$> f (postTitle p)
    where
      f = foldl (.) id (reverse $ fs <*> (pure s))

      fs :: [Page -> [Pandoc.Inline] -> [Pandoc.Inline]]
      fs = catMaybes $ buildPostTitle <$> processors b

toPostTitle :: Pandoc.Inline -> String
toPostTitle (Pandoc.Str s) = s
toPostTitle (Pandoc.Math _ s) = "$" ++ s ++ "$"
toPostTitle Pandoc.Space = " "
toPostTitle i = error $ "Unexpected inline in document title, got " ++ (show i)
