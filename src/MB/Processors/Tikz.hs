module MB.Processors.Tikz
    ( tikzProcessor
    )
where

import Control.Monad.Trans
import Data.List
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.Char8 (pack)
import System.Process
import System.Directory
import System.Exit
import qualified Text.Pandoc as Pandoc
import MB.Types
import qualified MB.Files as Files

tikzProcessor :: Processor
tikzProcessor =
    nullProcessor { preProcessPost = Just renderTikz
                  }

renderTikz :: Post -> BlogM Post
renderTikz post = do
  let Pandoc.Pandoc m blocks = postAst post
  newBlocks <- mapM (renderTikzScript post) blocks
  return $ post { postAst = Pandoc.Pandoc m newBlocks }

renderTikzScript :: Post
                 -> Pandoc.Block
                 -> BlogM Pandoc.Block
renderTikzScript post blk@(Pandoc.CodeBlock ("tikz", classes, _) rawScript) = do
  blog <- theBlog

  let digestInput = postTeXMacros post ++ rawScript

      -- Generate an image name in the images/ directory of the blog
      -- data directory.  Use a hash of the preamble name and script
      -- contents so we can avoid rendering the image again if it
      -- already exists.
      hash = showDigest $ sha1 $ pack digestInput
      imageFilename = "tikz-" ++ hash ++ ".png"
      imagePath = Files.imageFilename blog imageFilename
      preamble = unlines [ "\\documentclass{article}"
                         , "\\usepackage{tikz}"
                         , "\\usetikzlibrary{intersections,backgrounds,fit,calc,positioning}"
                         , "\\usepackage{pgfplots}"
                         , "\\usepackage{amsmath}"
                         , "\\pgfrealjobname{tmp}"

                         -- Include any TeX macro content in the
                         -- document preamble so it can be used in the
                         -- TikZ markup.
                         , postTeXMacros post

                         , "\\begin{document}"
                         , "\\begin{figure}"
                         , "\\beginpgfgraphicnamed{testfigure}"
                         , "\\begin{tikzpicture}"
                         ]
      postamble = unlines [ "\\end{tikzpicture}"
                          , "\\endpgfgraphicnamed"
                          , "\\end{figure}"
                          , "\\end{document}"
                          ]

      latexSource = preamble ++ rawScript ++ postamble

  let newBlock = Pandoc.Para [Pandoc.RawInline "html" $
                                    concat [ "<img src=\"/generated-images/"
                                           , imageFilename
                                           , "\" class=\""
                                           , intercalate " " classes
                                           , "\">"
                                           ]
                             ]

  e <- liftIO $ doesFileExist imagePath
  case e of
    True -> return newBlock
    False -> do
           liftIO $ writeFile "/tmp/tmp.tex" latexSource

           (s1, out1, _) <- liftIO $ readProcessWithExitCode "pdflatex" [ "-halt-on-error", "-output-directory", "/tmp"
                                                                        , "--jobname", "testfigure", "/tmp/tmp.tex"] ""
           case s1 of
             ExitFailure _ ->
                 liftIO $ do
                     putStrLn out1
                     putStrLn ">>>>>>> Input source >>>>>>>"
                     putStrLn latexSource
                     putStrLn ">>>>> End input source >>>>>"
                     return blk
             ExitSuccess -> do
                     -- Convert the temporary file to a PNG.
                     (s2, out2, err) <- liftIO $ readProcessWithExitCode "convert" [ "-density", "120"
                                                                                   , "-quality", "100"
                                                                                   , "/tmp/testfigure.pdf"
                                                                                   , imagePath
                                                                                   ] ""
                     case s2 of
                       ExitFailure _ ->
                           liftIO $ do
                               putStrLn "Could not render Tikz picture:"
                               putStrLn "Equation was:"
                               putStrLn latexSource
                               putStrLn "dvipng output:"
                               putStrLn out2
                               putStrLn err
                               return blk
                       ExitSuccess -> return newBlock

renderTikzScript _ b = return b