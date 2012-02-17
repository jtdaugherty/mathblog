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

applyPreProcessors :: Blog -> Post -> IO Post
applyPreProcessors b post = applyPreProcessors_ b post (processors b)

applyPostProcessors :: Blog -> FilePath -> IO ()
applyPostProcessors b path = applyPostProcessors_ b path (processors b)

applyPreProcessors_ :: Blog -> Post -> [Processor] -> IO Post
applyPreProcessors_ _ post [] = return post
applyPreProcessors_ b post (p:ps) = do
  post' <- case preProcessPost p of
             Nothing -> return post
             Just f -> f b post
  applyPreProcessors_ b post' ps

applyPostProcessors_ :: Blog -> FilePath -> [Processor] -> IO ()
applyPostProcessors_ _ _ [] = return ()
applyPostProcessors_ b pth (p:ps) = do
  case postProcessPost p of
    Nothing -> return ()
    Just f -> f b pth
  applyPostProcessors_ b pth ps

getWriterOptions :: Blog -> Pandoc.WriterOptions -> Pandoc.WriterOptions
getWriterOptions b = foldl (.) id (catMaybes $ applyWriterOptions <$> processors b)

getRawPostTitle :: Blog -> Post -> String
getRawPostTitle b p = head (fs ++ [fallback]) $ postTitle p
    where
      fallback = concat . (getInlineStr <$>)
      fs = catMaybes $ rawPostTitle <$> processors b

getInlineStr :: Pandoc.Inline -> String
getInlineStr (Pandoc.Str s) = s
getInlineStr (Pandoc.Math _ s) = s
getInlineStr Pandoc.Space = " "
getInlineStr i = error $ "Unexpected inline in document title, got " ++ (show i)

getPostTitle :: Blog -> Post -> TitleSetting -> String
getPostTitle b p s = concat $ getInlineStr <$> f (postTitle p)
    where
      f = foldl (.) id (reverse $ fs <*> (pure s))

      fs :: [TitleSetting -> [Pandoc.Inline] -> [Pandoc.Inline]]
      fs = catMaybes $ buildPostTitle <$> processors b