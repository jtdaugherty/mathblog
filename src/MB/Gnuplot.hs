module MB.Gnuplot
    ( gnuplotProcessor
    )
where

import Control.Monad
    ( forM
    , forM_
    , when
    )
import Control.Applicative
import Data.List
    ( intercalate
    , isSuffixOf
    )
import Data.Maybe
    ( catMaybes
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
import System.FilePath
    ( takeBaseName
    )
import System.Process
    ( readProcessWithExitCode
    )
import System.Exit
    ( ExitCode(..)
    )
import Data.Time.Clock (UTCTime)
import qualified Data.Set as Set
import qualified Text.Pandoc as Pandoc
import MB.Types
import qualified MB.Files as Files
import MB.Util

gnuplotProcessor :: Processor
gnuplotProcessor =
    nullProcessor { preProcessPost = Just renderGnuPlot
                  , getChangeSummary = Just checkPreambles
                  }

allPreambles :: FilePath -> IO [FilePath]
allPreambles preambleDir = do
  allFiles <- dirFilenames preambleDir
  return [ f | f <- allFiles
         , ".txt" `isSuffixOf` f
         ]

checkPreambles :: Blog -> UTCTime -> IO ChangeSummary
checkPreambles blog baseTime = do
  filenames <- allPreambles $ eqPreamblesDir blog

  -- Have any preambles changed?
  mtimes <- forM filenames $ \f -> (,) <$> pure f <*> getModificationTime f
  case filter ((> baseTime) . snd) mtimes of
    [] -> return noChanges
    results -> do
      let changedPreambles = takeBaseName <$> fst <$> results

      -- If so, find all posts with gnuplot equations in them and
      -- schedule them for rendering.
      --
      -- Only rebuild posts if they *actually* use gnuplot (i.e., only
      -- posts whose code blocks reference gnuplot preambles.
      let usedPreambles p = (Set.fromList $ postPreambles p)
                            `Set.intersection` (Set.fromList changedPreambles)
          ps = filter (not . Set.null . usedPreambles) $ blogPosts blog
          allUsedPreambles = Set.unions $ usedPreambles <$> ps

      when (not $ null ps) $
           do
             putStrLn $ "[gnuplot] some preambles changed:"
             putStrLn $ "[gnuplot]   " ++ (intercalate " " $ Set.toList allUsedPreambles)
             putStrLn $ "[gnuplot] used by:"
             forM_ ps $ \p -> putStrLn $ "[gnuplot]   " ++ postFilename p

      return $ noChanges { postsChanged = postFilename <$> ps }

-- Return which gnuplot preambles, if any, are referenced by code
-- blocks.
postPreambles :: Post -> [String]
postPreambles p = catMaybes $ getP <$> bs
    where bs = getBlocks $ postAst p
          getP b = if isCodeBlock b
                   then let Pandoc.CodeBlock (preambleName, _, _) _ = b
                        in Just preambleName
                   else Nothing

renderGnuPlot :: Blog -> Post -> IO Post
renderGnuPlot config post = do
  newBlocks <- forM (getBlocks $ postAst post) $ \blk ->
               if isCodeBlock blk
               then processCodeBlock config blk
               else return blk

  return $ post { postAst = putBlocks (postAst post) newBlocks }

getBlocks :: Pandoc.Pandoc -> [Pandoc.Block]
getBlocks (Pandoc.Pandoc _ bs) = bs

putBlocks :: Pandoc.Pandoc -> [Pandoc.Block] -> Pandoc.Pandoc
putBlocks (Pandoc.Pandoc m _) bs = Pandoc.Pandoc m bs

isCodeBlock :: Pandoc.Block -> Bool
isCodeBlock (Pandoc.CodeBlock _ _) = True
isCodeBlock _ = False

processCodeBlock :: Blog -> Pandoc.Block -> IO Pandoc.Block
processCodeBlock config (Pandoc.CodeBlock (preambleName, classes, _) s) =
    renderGnuPlotScript config preambleName s classes
processCodeBlock _ b = return b

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
