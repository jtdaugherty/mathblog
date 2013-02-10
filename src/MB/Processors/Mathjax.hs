module MB.Processors.Mathjax
    ( mathjaxProcessor
    )
where

import qualified Text.Pandoc as Pandoc
import MB.Types

mathjaxProcessor :: Processor
mathjaxProcessor =
    nullProcessor { applyWriterOptions = Just mathjaxOpts
                  , pageHead = Just mathjaxPageHead
                  }

mathjaxOpts :: Pandoc.WriterOptions -> Pandoc.WriterOptions
mathjaxOpts opts =
    opts { Pandoc.writerHTMLMathMethod = Pandoc.MathJax "MathJax/MathJax.js"
         }

mathjaxPageHead :: String
mathjaxPageHead =
    "<script type=\"text/x-mathjax-config\">\n\
    \    MathJax.Hub.Config(\n\
    \      {\"HTML-CSS\": { preferredFont: \"TeX\", availableFonts: [\"STIX\",\"TeX\"] },\n\
    \       tex2jax: {\n\
    \         element: null,\n\
    \         preview: \"none\",\n\
    \         skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],\n\
    \         inlineMath: [\n\
    \           ['$', '$'],\n\
    \           [\"\\\\(\", \"\\\\)\"],\n\
    \         ],\n\
    \         displayMath: [\n\
    \           ['$$', '$$'],\n\
    \           [\"\\\\[\", \"\\\\]\"],\n\
    \         ],\n\
    \         processEscapes: true,\n\
    \         ignoreClass: \"tex2jax_ignore|dno\"\n\
    \       },\n\
    \       TeX: {\n\
    \         equationNumbers: { autoNumber: \"AMS\" },\n\
    \         noUndefined: { attributes: {\n\
    \                          mathcolor: \"red\",\n\
    \                          mathbackground: \"#FFEEEE\",\n\
    \                          mathsize: \"90%\" }\n\
    \                      }\n\
    \       },\n\
    \       messageStyle: \"none\"\n\
    \    });\n\
    \</script>\n\
    \<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML\"></script>\n\
    \<script type=\"text/javascript\">\n\
    \  // Uncomment this code and set the style of the \"page\" element\n\
    \  // below to visibility: hidden initially if you don't like the\n\
    \  // MathJax flicker during the rendering process. :(\n\
    \  // MathJax.Hub.Queue(function () {\n\
    \  //     document.getElementById(\"page\").style.visibility = \"visible\";\n\
    \  // });\n\
    \</script>\n"