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

applyPreProcessors :: Blog -> Post -> IO Post
applyPreProcessors b post = applyPreProcessors_ b post (processors b)

applyPostProcessors :: Blog -> FilePath -> Page -> IO ()
applyPostProcessors b path pg = applyPostProcessors_ b path pg (processors b)

applyPreProcessors_ :: Blog -> Post -> [Processor] -> IO Post
applyPreProcessors_ _ post [] = return post
applyPreProcessors_ b post (p:ps) = do
  post' <- case preProcessPost p of
             Nothing -> return post
             Just f -> f b post
  applyPreProcessors_ b post' ps

applyPostProcessors_ :: Blog -> FilePath -> Page -> [Processor] -> IO ()
applyPostProcessors_ _ _ _ [] = return ()
applyPostProcessors_ b pth pg (p:ps) = do
  case postProcessPost p of
    Nothing -> return ()
    Just f -> f b pth pg
  applyPostProcessors_ b pth pg ps

getWriterOptions :: Blog -> Pandoc.WriterOptions -> Pandoc.WriterOptions
getWriterOptions b = foldl (.) id (catMaybes $ applyWriterOptions <$> processors b)

getRawPostTitle :: Blog -> Post -> String
getRawPostTitle b p = head (fs ++ [fallback]) $ postTitle p
    where
      fallback = concat . (getInlineStr <$>)
      fs = catMaybes $ rawPostTitle <$> processors b

getPostTitle :: Blog -> Post -> Page -> String
getPostTitle b p s = concat $ getInlineStr <$> f (postTitle p)
    where
      f = foldl (.) id (reverse $ fs <*> (pure s))

      fs :: [Page -> [Pandoc.Inline] -> [Pandoc.Inline]]
      fs = catMaybes $ buildPostTitle <$> processors b