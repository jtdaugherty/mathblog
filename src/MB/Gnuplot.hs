module MB.Gnuplot
    ( gnuplotProcessor
    )
where

import Control.Monad
    ( forM
    )
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
import System.Directory
    ( doesFileExist
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

gnuplotProcessor :: Processor
gnuplotProcessor =
    nullProcessor { preProcessPost = Just renderGnuPlot
                  }

renderGnuPlot :: Blog -> Post -> IO Post
renderGnuPlot config post = do
  let Pandoc.Pandoc m blocks = postAst post
  newBlocks <- forM blocks $ \blk ->
               case blk of
                 Pandoc.CodeBlock (preambleName, classes, _) s ->
                     renderGnuPlotScript config preambleName s classes
                 b -> return b

  return $ post { postAst = Pandoc.Pandoc m newBlocks }

loadPreamble :: Blog -> String -> IO (Maybe String)
loadPreamble config preambleName = do
  let filename = Files.eqPreambleFile config $ preambleName ++ ".txt"
  e <- doesFileExist filename
  case e of
    False -> return Nothing
    True -> do
           s <- readFile filename
           s `seq` return ()
           return $ Just s

renderGnuPlotScript :: Blog
                    -> String
                    -> String
                    -> [String]
                    -> IO Pandoc.Block
renderGnuPlotScript config preambleName rawScript classes = do
  putStrLn $ "Rendering equation graph, type=" ++ preambleName

  mPreamble <- loadPreamble config preambleName

  case mPreamble of
    Nothing -> do
      putStrLn $ "Error: no such gnuplot preamble: " ++ preambleName
      return $ Pandoc.Para [Pandoc.Str "[[COULD NOT DRAW EQUATION]]"]
    Just preamble -> do

      let scriptLines = lines rawScript
          preambleLines = lines preamble
          digestInput = preambleName ++ rawScript

      -- Generate an image name in the images/ directory of the blog
      -- data directory.  Use a hash of the preamble name and script
      -- contents so we can avoid rendering the image again if it
      -- already exists.
      let hash = showDigest $ sha1 $ pack digestInput
          imageFilename = preambleName ++ "-" ++ hash ++ ".png"
          imagePath = Files.imageFilename config imageFilename
          outputLines = [ "set term png enhanced"
                        , "set output \"" ++ imagePath ++ "\""
                        ]
          fullScript = intercalate "; " $ outputLines ++ preambleLines ++ scriptLines

      -- Invoke gnuplot to render the image
      (status, out, err) <- readProcessWithExitCode "gnuplot" ["-e", fullScript] ""

      case status of
        ExitSuccess -> return ()
        ExitFailure _ -> do
                       putStrLn "Could not render equation:"
                       putStrLn "Equation was:"
                       putStrLn rawScript
                       putStrLn "gnuplot output:"
                       putStrLn out
                       putStrLn err

      return $ Pandoc.Para [Pandoc.RawInline "html" $ concat [ "<img src=\"/generated-images/"
                                                       , imageFilename
                                                       , "\" class=\""
                                                       , intercalate " " classes
                                                       , "\">"
                                                       ]
                           ]
