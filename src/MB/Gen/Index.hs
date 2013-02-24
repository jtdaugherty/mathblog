module MB.Gen.Index
    ( buildIndexPage
    )
where

import Control.Monad
import Control.Monad.Trans
import System.Directory

import MB.Types
import qualified MB.Files as Files

buildIndexPage :: BlogM ()
buildIndexPage = do
  blog <- theBlog

  when (null $ blogPosts blog) $
       error "No blog posts; please create a first post in posts/."

  let src = Files.postFinalHtml blog post
      index = Files.indexHtml blog
      post = head $ blogPosts blog

  liftIO $ copyFile src index
