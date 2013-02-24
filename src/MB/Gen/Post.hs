{-# LANGUAGE ScopedTypeVariables #-}
module MB.Gen.Post
    ( generatePosts
    , postTemplateAttrs
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
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
            setManyAttrib [ ("next_post_url", postUrl <$> (fst nextPrev))
                          , ("prev_post_url", postUrl <$> (snd nextPrev))
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
                  , ("url", postUrl post)
                  , ("basename", postBaseName post)
                  , ("tex_macros", postTeXMacros post)
                  ]

generatePosts :: BlogM ()
generatePosts = do
  blog <- theBlog

  let toRender = zip (blogPosts blog) [0..]

  forM_ toRender $ \(post, pos) ->
      renderSingle post pos

renderSingle :: Post -> Int -> BlogM ()
renderSingle post pos = do
  blog <- theBlog
  conf <- theConfig

  let renderCauses p =
          catMaybes $ [ if postModificationTime p > baselineMTime blog
                        then Just PostModified
                        else Nothing
                      , if postIndexMTime blog > baselineMTime blog
                        then Just PostIndex
                        else Nothing
                      , if configMTime blog > baselineMTime blog
                        then Just Config
                        else Nothing
                      , if forceRegeneration conf
                        then Just Forced
                        else Nothing
                      , if templateMTime blog > baselineMTime blog
                        then Just Template
                        else Nothing
                      ]

  if null (renderCauses post) then
      return () else
      do
        let nextPost = if pos == 0
                       then Nothing
                       else Just (blogPosts blog !! (pos - 1))

            prevPost = if pos == (length $ blogPosts blog) - 1
                       then Nothing
                       else Just (blogPosts blog !! (pos + 1))

        notify $ PostRender post $ renderCauses post

        -- Steps:

        -- Transform AST with preprocessors;
        newPost <- applyPreProcessors post

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
                  liftIO $ writeFile (Files.postFinalHtml blog post) finalPageHtml

                  -- Give postprocessors a chance to transform the
                  -- final HTML.
                  applyPostProcessors (Files.postFinalHtml blog post) BlogPost
