module MB.Gen.Index
    ( buildIndexPage
    )
where

import Control.Monad
import System.Directory

import MB.Types
import qualified MB.Files as Files

buildIndexPage :: Blog -> IO ()
buildIndexPage blog = do
  when (null $ blogPosts blog) $
       error "No blog posts; please create a first post in posts/."

  let src = Files.postFinalHtml blog post
      index = Files.indexHtml blog
      post = head $ blogPosts blog

  copyFile src index
