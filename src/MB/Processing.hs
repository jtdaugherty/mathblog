module MB.Processing
    ( applyProcessors
    , getWriterOptions
    )
where

import Control.Applicative ((<$>))
import qualified Text.Pandoc as Pandoc
import MB.Types

applyProcessors :: Blog -> Post -> IO Post
applyProcessors b post = applyProcessors_ b post (processors b)

applyProcessors_ :: Blog -> Post -> [Processor] -> IO Post
applyProcessors_ _ post [] = return post
applyProcessors_ b post (p:ps) = do
  post' <- processPost p b post
  applyProcessors_ b post' ps

getWriterOptions :: Blog -> Pandoc.WriterOptions -> Pandoc.WriterOptions
getWriterOptions b = foldl (.) id (applyWriterOptions <$> processors b)
