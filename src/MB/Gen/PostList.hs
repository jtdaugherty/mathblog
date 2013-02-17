module MB.Gen.PostList
    ( generatePostList
    )
where

import System.IO
import System.Exit
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
  eTmpl <- loadTemplate $ Files.listTemplatePath blog

  case eTmpl of
    Left msg -> putStrLn msg >> exitFailure
    Right tmpl ->
        do
          postData <- mapM (postTemplateAttrs blog) (blogPosts blog)

          let tmpl' = setManyAttrib [("posts", postData)] tmpl
              out = fillTemplate blog tmpl' []

          h <- openFile (Files.listHtml blog) WriteMode
          buildPage h blog out Nothing
          hClose h

          applyPostProcessors blog (Files.listHtml blog) Index
