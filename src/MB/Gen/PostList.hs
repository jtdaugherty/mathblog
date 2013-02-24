module MB.Gen.PostList
    ( generatePostList
    )
where

import Control.Applicative
import Control.Monad.Trans
import Text.StringTemplate
    ( setManyAttrib )

import MB.Types
import MB.Templates
import MB.Processing
import MB.Gen.Base
import MB.Gen.Post (postTemplateAttrs)

generatePostList :: BlogM ()
generatePostList = do
  blog <- theBlog
  let st = inputFS blog

  withTemplate (ifsListTemplatePath st) $ \listTmpl ->
      withTemplate (ifsPageTemplatePath st) $ \pageTmpl ->
          do
            let postData = postTemplateAttrs blog <$> blogPosts blog
                listTmpl' = setManyAttrib [("posts", postData)] listTmpl
                out = fillTemplate blog listTmpl' []

            liftIO $ writeFile (ofsListHtml $ outputFS blog) $
              buildPage blog out Nothing pageTmpl
            applyPostProcessors (ofsListHtml $ outputFS blog) Index
