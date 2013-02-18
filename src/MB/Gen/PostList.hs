module MB.Gen.PostList
    ( generatePostList
    )
where

import Control.Applicative
import Text.StringTemplate
    ( setManyAttrib )

import qualified MB.Files as Files
import MB.Types
import MB.Templates
import MB.Processing
import MB.Gen.Base
import MB.Gen.Post (postTemplateAttrs)

generatePostList :: Blog -> IO ()
generatePostList blog = do
  withTemplate (Files.listTemplatePath blog) $ \listTmpl ->
      withTemplate (Files.pageTemplatePath blog) $ \pageTmpl ->
          do
            let postData = postTemplateAttrs blog <$> blogPosts blog
                listTmpl' = setManyAttrib [("posts", postData)] listTmpl
                out = fillTemplate blog listTmpl' []

            writeFile (Files.listHtml blog) $ buildPage blog out Nothing pageTmpl
            applyPostProcessors blog (Files.listHtml blog) Index
