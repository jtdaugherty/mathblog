{-# LANGUAGE ScopedTypeVariables #-}
module MB.Gen.Post
    ( generatePosts
    , postTemplateAttrs
    )
where

import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Text.StringTemplate
    ( setManyAttrib )

import qualified Text.Pandoc as Pandoc
import MB.Processing
import MB.Types
import MB.Templates
import qualified MB.Files as Files

import MB.Gen.Base

renderPostTemplate :: Blog -> Post -> (Maybe Post, Maybe Post) -> Template -> String -> String
renderPostTemplate blog post nextPrev postTemplate postHtml =
    let pAttrs = postTemplateAttrs blog post
        tmplWithPostAttrs =
            setManyAttrib [("post_authors", postAuthors post)] $
            setManyAttrib [("post", pAttrs)] $
            setManyAttrib [ ("next_post_url", Files.postUrl <$> (fst nextPrev))
                          , ("prev_post_url", Files.postUrl <$> (snd nextPrev))
                          ]
            postTemplate

        attrs = [ ("post_html", postHtml)
                ]
    in fillTemplate blog tmplWithPostAttrs attrs

postTemplateAttrs :: Blog -> Post -> M.Map String String
postTemplateAttrs blog post =
    let datestr = postDate post <|> Just (postModificationString post)
    in M.fromList [ ("title", getPostTitle blog post BlogPost)
                  , ("date", fromJust datestr)
                  , ("url", Files.postUrl post)
                  , ("basename", Files.postBaseName post)
                  , ("tex_macros", postTeXMacros post)
                  ]

generatePosts :: Blog -> [Post] -> IO ()
generatePosts blog posts = do
  let n = length posts

  when (n > 0) $
       putStrLn "Rendering post(s):"

  let allPosts = zip (blogPosts blog) [0..]
      toRender = [ (p, pos) | (p, pos) <- allPosts
                 , postFilename p `elem` (postFilename <$> posts)
                 ]

  forM_ (zip toRender [1..]) $ \((post, pos), i::Int) ->
      do
        putStrLn $ "  [" ++ (show i) ++ "/" ++
                     (show n) ++ "] " ++ (postFilename post)
        putStrLn $ "        title: \"" ++ (getPostTitle blog post Index) ++ "\""

        let nextPost = if pos == 0
                       then Nothing
                       else Just (blogPosts blog !! (pos - 1))

            prevPost = if pos == (length $ blogPosts blog) - 1
                       then Nothing
                       else Just (blogPosts blog !! (pos + 1))

        -- Steps:

        -- Transform AST with preprocessors;
        newPost <- applyPreProcessors blog post

        -- Render the transformed AST as HTML using Pandoc;
        let writerOpts = getWriterOptions blog Pandoc.defaultWriterOptions
            postBodyHtml = Pandoc.writeHtmlString writerOpts (postAst newPost)

        withTemplate (Files.pageTemplatePath blog) $ \pageTmpl ->
            withTemplate (Files.postTemplatePath blog) $ \postTmpl ->
                do
                  -- Embed the converted Pandoc HTML into the postTemplate;
                  let postPageHtml = renderPostTemplate blog post (nextPost, prevPost) postTmpl postBodyHtml
                      -- Embed the postTemplate result in the pageTemplate.
                      finalPageHtml = buildPage blog postPageHtml (Just $ getRawPostTitle blog post) pageTmpl

                  -- Write the final, complete page HTML to the post
                  -- HTML file location.
                  writeFile (Files.postFinalHtml blog post) finalPageHtml

                  -- Give postprocessors a chance to transform the
                  -- final HTML.
                  applyPostProcessors blog (Files.postFinalHtml blog post) BlogPost
