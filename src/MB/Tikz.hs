module MB.Tikz
    ( tikzProcessor
    )
where

import Data.List
    ( intercalate
    )
import Data.Digest.Pure.SHA
    ( showDigest
    , sha1
    )
import Data.ByteString.Lazy.Char8
    ( pack
    )
import System.Process
    ( readProcessWithExitCode
    )
import System.Exit
    ( ExitCode(..)
    )
import qualified Text.Pandoc as Pandoc
import MB.Types
import qualified MB.Files as Files

tikzProcessor :: Processor
tikzProcessor =
    nullProcessor { preProcessPost = Just renderTikz
                  }

renderTikz :: Blog -> Post -> IO Post
renderTikz config post = do
  let Pandoc.Pandoc m blocks = postAst post
  newBlocks <- mapM (renderTikzScript config) blocks
  return $ post { postAst = Pandoc.Pandoc m newBlocks }

renderTikzScript :: Blog
                 -> Pandoc.Block
                 -> IO Pandoc.Block
renderTikzScript config (Pandoc.CodeBlock ("tikz", classes, _) rawScript) = do
  putStrLn "Rendering equation graph (tikz)"

  let digestInput = rawScript

      -- Generate an image name in the images/ directory of the blog
      -- data directory.  Use a hash of the preamble name and script
      -- contents so we can avoid rendering the image again if it
      -- already exists.
      hash = showDigest $ sha1 $ pack digestInput
      imageFilename = "tikz-" ++ hash ++ ".png"
      imagePath = Files.imageFilename config imageFilename
      preamble = unlines [ "\\documentclass{article}"
                         , "\\pagestyle{empty}"
                         , "\\addtolength{\\textheight}{1.75in}"
                         , "\\addtolength{\\topmargin}{-1.2in}"
                         , "\\setlength{\\headheight}{0in}"
                         , "\\setlength{\\headsep}{0in}"
                         , "\\setlength{\\topskip}{0in}"
                         , "\\addtolength{\\textwidth}{1.75in}"
                         , "\\addtolength{\\oddsidemargin}{-2in}"
                         , "\\usepackage{tikz}"
                         , "\\usepackage{pgfplots}"
                         , "\\begin{document}"
                         , "\\begin{tikzpicture}"
                         ]
      postamble = unlines [ "\\end{tikzpicture}"
                          , "\\end{document}"
                          ]

      latexSource = preamble ++ rawScript ++ postamble

  -- Write the latex source to a temporary file.
  writeFile "/tmp/tmp.tex" latexSource

  -- Have tex write the DVI to a temporary file.
  (_, out, _) <- readProcessWithExitCode "pdflatex" ["/tmp/tmp.tex"] ""
  putStrLn out

  -- Convert the temporary file to a PNG.
  (status, out2, err) <- readProcessWithExitCode "convert" [ "-density", "120"
                                                           , "-quality", "100"
                                                           , "tmp.pdf"
                                                           , imagePath
                                                           ] ""

  case status of
    ExitSuccess -> return ()
    ExitFailure _ -> do
                putStrLn "Could not render Tikz picture:"
                putStrLn "Equation was:"
                putStrLn latexSource
                putStrLn "dvipng output:"
                putStrLn out2
                putStrLn err

  return $ Pandoc.Para [Pandoc.RawInline "html" $
                              concat [ "<img src=\"" ++ baseUrl config ++ "/generated-images/"
                                     , imageFilename
                                     , "\" class=\""
                                     , intercalate " " classes
                                     , "\">"
                                     ]
                       ]
renderTikzScript _ b = return b