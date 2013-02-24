module MB.Processors.Gnuplot
    ( gnuplotProcessor
    )
where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Digest.Pure.SHA
import Data.ByteString.Lazy.Char8
    ( pack )
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Paths_mathblog
    ( getDataFileName )
-- import Data.Time.Clock (UTCTime)
-- import qualified Data.Set as Set
import qualified Text.Pandoc as Pandoc
import MB.Types
import qualified MB.Files as Files
import MB.Util

gnuplotProcessor :: Processor
gnuplotProcessor =
    nullProcessor { preProcessPost = Just renderGnuPlot
                  , checkDataDir = Just setupGnuplot
                  }

allPreambles :: FilePath -> IO [FilePath]
allPreambles preambleDir = do
  allFiles <- dirFilenames preambleDir
  return [ f | f <- allFiles
         , ".txt" `isSuffixOf` f
         ]

setupGnuplot :: BlogM ()
setupGnuplot = do
  blog <- theBlog
  liftIO $ do
    srcDir <- getDataFileName "gnuplot-templates"
    let dest = baseDir blog </> "eq-preambles"

    destExists <- doesDirectoryExist dest
    when (not destExists) $ copyTree srcDir dest

eqPreamblesDir :: Blog -> FilePath
eqPreamblesDir b = baseDir b </> "eq-preambles"

eqPreambleFile :: Blog -> String -> FilePath
eqPreambleFile config n = eqPreamblesDir config </> n

-- checkPreambles :: Blog -> UTCTime -> IO ChangeSummary
-- checkPreambles blog baseTime = do
--   filenames <- allPreambles $ eqPreamblesDir blog

--   -- Have any preambles changed?
--   mtimes <- forM filenames $ \f -> (,) <$> pure f <*> getModificationTime f
--   case filter ((> baseTime) . snd) mtimes of
--     [] -> return noChanges
--     results -> do
--       let changedPreambles = takeBaseName <$> fst <$> results

--       -- If so, find all posts with gnuplot equations in them and
--       -- schedule them for rendering.
--       --
--       -- Only rebuild posts if they *actually* use gnuplot (i.e., only
--       -- posts whose code blocks reference gnuplot preambles.
--       let usedPreambles p = (Set.fromList $ postPreambles p)
--                             `Set.intersection` (Set.fromList changedPreambles)
--           ps = filter (not . Set.null . usedPreambles) $ blogPosts blog
--           allUsedPreambles = Set.unions $ usedPreambles <$> ps

--       when (not $ null ps) $
--            do
--              putStrLn $ "[gnuplot] some preambles changed:"
--              putStrLn $ "[gnuplot]   " ++ (intercalate " " $ Set.toList allUsedPreambles)
--              putStrLn $ "[gnuplot] used by:"
--              forM_ ps $ \p -> putStrLn $ "[gnuplot]   " ++ postFilename p

--       return $ noChanges { postsChanged = postFilename <$> ps }

-- Return which gnuplot preambles, if any, are referenced by code
-- blocks.
postPreambles :: Post -> [String]
postPreambles p = catMaybes $ getP <$> bs
    where bs = getBlocks $ postAst p
          getP b = if isCodeBlock b
                   then let Pandoc.CodeBlock (preambleName, _, _) _ = b
                        in Just preambleName
                   else Nothing

renderGnuPlot :: Post -> BlogM Post
renderGnuPlot post = do
  newBlocks <- forM (getBlocks $ postAst post) $ \blk ->
               if isCodeBlock blk
               then processCodeBlock blk
               else return blk

  return $ post { postAst = putBlocks (postAst post) newBlocks }

getBlocks :: Pandoc.Pandoc -> [Pandoc.Block]
getBlocks (Pandoc.Pandoc _ bs) = bs

putBlocks :: Pandoc.Pandoc -> [Pandoc.Block] -> Pandoc.Pandoc
putBlocks (Pandoc.Pandoc m _) bs = Pandoc.Pandoc m bs

isCodeBlock :: Pandoc.Block -> Bool
isCodeBlock (Pandoc.CodeBlock _ _) = True
isCodeBlock _ = False

processCodeBlock :: Pandoc.Block -> BlogM Pandoc.Block
processCodeBlock b@(Pandoc.CodeBlock (preambleName, classes, _) s) = do
  result <- renderGnuPlotScript preambleName s classes
  return $ fromJust $ result <|> Just b
processCodeBlock b = return b

loadPreamble :: String -> BlogM (Maybe String)
loadPreamble preambleName = do
  blog <- theBlog

  let filename = eqPreambleFile blog $ preambleName ++ ".txt"
  e <- liftIO $ doesFileExist filename
  case e of
    False -> return Nothing
    True -> do
           s <- liftIO $ readFile filename
           s `seq` return ()
           return $ Just s

renderGnuPlotScript :: String
                    -> String
                    -> [String]
                    -> BlogM (Maybe Pandoc.Block)
renderGnuPlotScript preambleName rawScript classes = do
  blog <- theBlog
  mPreamble <- loadPreamble preambleName

  case mPreamble of
    Nothing -> return Nothing
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
          imagePath = Files.imageFilename blog imageFilename
          outputLines = [ "set term png enhanced"
                        , "set output \"" ++ imagePath ++ "\""
                        ]
          fullScript = intercalate "; " $ outputLines ++ preambleLines ++ scriptLines

      -- Invoke gnuplot to render the image
      (status, out, err) <- liftIO $ readProcessWithExitCode "gnuplot" ["-e", fullScript] ""

      case status of
        ExitSuccess -> return ()
        ExitFailure _ -> liftIO $ do
                           putStrLn "Could not render equation:"
                           putStrLn "Equation was:"
                           putStrLn rawScript
                           putStrLn "gnuplot output:"
                           putStrLn out
                           putStrLn err

      return $ Just $
             Pandoc.Para [Pandoc.RawInline "html" $
                                concat [ "<img src=\"/generated-images/"
                                       , imageFilename
                                       , "\" class=\""
                                       , intercalate " " classes
                                       , "\">"
                                       ]
                         ]
