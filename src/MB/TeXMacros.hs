module MB.TeXMacros
    ( extractTeXMacros
    )
where

import Control.Applicative
import Data.Either
import qualified Text.Pandoc as Pandoc

-- |Extract "#tex-macros" blocks from the page and store their
-- contents in the postTeXMacros field.  This removes the tex-macros
-- nodes from the Pandoc AST; see the Mathjax and Tikz processors to
-- see how they eventually make it back into the right spots in the
-- output.
extractTeXMacros :: [Pandoc.Block]
                 -> ([Pandoc.Block], String)
extractTeXMacros blocks =
    let result = gatherMacros <$> blocks
    in (lefts result, concat $ rights result)

gatherMacros :: Pandoc.Block
             -> Either Pandoc.Block String
gatherMacros (Pandoc.CodeBlock ("tex-macros", _, _) rawScript) = Right rawScript
gatherMacros b = Left b
