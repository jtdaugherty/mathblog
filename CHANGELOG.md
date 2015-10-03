0.6
---

 * Added support for building with GHC 7.10
 * doc: update manual-generating scripts to assume "mb" is installed a
   cabal sandbox
 * Propagate Pandoc loading errors to top level via EitherT
 * Fix read-then-close (lazy IO) (thanks Thomas M. DuBuisson
   (thomas.dubuisson@gmail.com))
 * Un-bitrot (wrt system-filepath and old-locale/pandoc) (thanks Thomas
   M. DuBuisson (thomas.dubuisson@gmail.com))
 * Update to use Pandoc 1.12, JuicyPixels 3.1
 * Render images at double DPI, then shrink in the HTML (read: support
   retina displays!)
 * Stop using greadlink in manual scripts
 * Move Mathjax configuration from Haskell code to page template (fixes #11)
 * Add browser reloading support to blog regeneration
 * Add built-in HTTP server in listen mode
 * manual: actually run "mb" to generate "mb" output in the manual
 * Discontinue support for Gnuplot
 * Start to use fsnotify
 * Added manual
 * mathjax: include cancel extension
 * bugfix: use correct indexing when generating next/prev post links
 * Remove support for gladtex
 * bugfix: only consider .html files when checking for modified templates
 * Feature: parse date from blog post header and include in RSS feed
 * Feature: add support for page-wide TeX macro blocks (#tex-macros) for
   both Mathjax and TikZ
 * Add more packages to TikZ preamble, enable AMS extensions in mathjax
 * Mathjax: turn on AMS equation numbering, also print tikz source on
   tikz failure
 * tikz: add calc tikzlibrary
 * Rename -h/--html-dir to -o/--output-dir, add -h/--help and -v/--version
 * CHANGE: rename MB_BASE_DIR to MB_DATA_DIR, --baseDir to --data-dir;
   also remove default output directory behavior
 * Fixed typos in the documentation (thanks Peter Simons
   (peter.1.simons@nokia.com))
