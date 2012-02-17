module MB.Processing
    ( applyProcessors
    , getWriterOptions
    , getRawPostTitle
    , getPostTitle
    )
where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Maybe (catMaybes)
import qualified Text.Pandoc as Pandoc
import MB.Types

applyProcessors :: Blog -> Post -> IO Post
applyProcessors b post = applyProcessors_ b post (processors b)

applyProcessors_ :: Blog -> Post -> [Processor] -> IO Post
applyProcessors_ _ post [] = return post
applyProcessors_ b post (p:ps) = do
  post' <- case processPost p of
             Nothing -> return post
             Just f -> f b post
  applyProcessors_ b post' ps

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