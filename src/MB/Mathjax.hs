module MB.Mathjax
    ( mathjaxProcessor
    )
where

import qualified Text.Pandoc as Pandoc
import MB.Types

mathjaxProcessor :: Processor
mathjaxProcessor =
    Processor { applyWriterOptions = mathjaxOpts
              , processPost = \_ p -> return p
              }

mathjaxOpts :: Pandoc.WriterOptions -> Pandoc.WriterOptions
mathjaxOpts opts =
    opts { Pandoc.writerHTMLMathMethod = Pandoc.MathJax "MathJax/MathJax.js"
         }