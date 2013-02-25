module MB.Processors.Mathjax
    ( mathjaxProcessor
    )
where

import qualified Text.Pandoc as Pandoc
import MB.Types

mathjaxProcessor :: Processor
mathjaxProcessor =
    nullProcessor { applyWriterOptions = Just mathjaxOpts
                  }

mathjaxOpts :: Pandoc.WriterOptions -> Pandoc.WriterOptions
mathjaxOpts opts =
    opts { Pandoc.writerHTMLMathMethod = Pandoc.MathJax "MathJax/MathJax.js"
         }
