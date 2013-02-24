module MB.Gen.PostList
    ( generatePostList
    )
where

import Control.Applicative
import Control.Monad.Trans
import Text.StringTemplate
    ( setManyAttrib )

import qualified MB.Files as Files
import MB.Types
import MB.Templates
import MB.Processing
import MB.Gen.Base
import MB.Gen.Post (postTemplateAttrs)

generatePostList :: BlogM ()
generatePostList = do
  blog <- theBlog

  withTemplate (Files.listTemplatePath blog) $ \listTmpl ->
      withTemplate (Files.pageTemplatePath blog) $ \pageTmpl ->
          do
            let postData = postTemplateAttrs blog <$> blogPosts blog
                listTmpl' = setManyAttrib [("posts", postData)] listTmpl
                out = fillTemplate blog listTmpl' []

            liftIO $ writeFile (Files.listHtml blog) $
              buildPage blog out Nothing pageTmpl
            applyPostProcessors (Files.listHtml blog) Index
