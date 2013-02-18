module MB.Gen.Post
    ( generateChangedPosts
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
renderPostTemplate blog post prevNext postTemplate postHtml =
    let pAttrs = postTemplateAttrs blog post
        tmplWithPostAttrs =
            setManyAttrib [("post_authors", postAuthors post)] $
            setManyAttrib [("post", pAttrs)] $
            setManyAttrib [ ("next_post_url", Files.postUrl <$> (fst prevNext))
                          , ("prev_post_url", Files.postUrl <$> (snd prevNext))
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

generateChangedPosts :: Blog -> ChangeSummary -> IO ()
generateChangedPosts blog summary = do
  let numRegenerated = if configChanged summary
                       then length $ blogPosts blog
                       else length $ postsChanged summary

  when (numRegenerated > 0) $ putStrLn $ "Rendering " ++ (show numRegenerated) ++ " post" ++
       (if numRegenerated == 1 then "" else "s") ++ ":"

  let n = length posts
      posts = [ p | p <- blogPosts blog
              , postFilename p `elem` postsChanged summary ||
                             postIndexChanged summary
              ]

  forM_ (zip posts [0..]) $ \(post, i) ->
      do
        putStrLn $ "Rendering post " ++ (show $ i + 1) ++ "/" ++
                     (show numRegenerated) ++ ": " ++ (getPostTitle blog post Index) ++
                     " (" ++ (postFilename post) ++ ")"

        let prevPost = if i == 0 then Nothing else Just (posts !! (i - 1))
            nextPost = if i == n - 1 then Nothing else Just (posts !! (i + 1))

        -- Steps:

        -- Process AST
        newPost <- applyPreProcessors blog post

        -- Render that AST as HTML using Pandoc
        let writerOpts = getWriterOptions blog Pandoc.defaultWriterOptions
            postBodyHtml = Pandoc.writeHtmlString writerOpts (postAst newPost)

        -- Embed the Pandoc into the postTemplate to generate the main
        -- page body
        withTemplate (Files.pageTemplatePath blog) $ \pageTmpl ->
            withTemplate (Files.postTemplatePath blog) $ \postTmpl ->
                do
                  let postPageHtml = renderPostTemplate blog post (prevPost, nextPost) postTmpl postBodyHtml
                      finalPageHtml = buildPage blog postPageHtml (Just $ getRawPostTitle blog post) pageTmpl
                  writeFile (Files.postFinalHtml blog post) finalPageHtml
                  applyPostProcessors blog (Files.postFinalHtml blog post) BlogPost
