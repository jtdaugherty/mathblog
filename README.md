mathblog
========

mathblog is a Haskell program targeted at people who want to write
statically-generated, mathematically-themed weblogs.  It supports:

 - Extended Markdown input syntax as supported by the Pandoc library

 - Inline and block-level TeX math rendered by MathJax or LaTeX

 - Function graphing with TikZ / pgfplots LaTeX packages

 - Integration of Javascript-based web services such as Disqus

 - Template-based document rendering with support for layout and style
   customization

Getting Started
===============

See the manual PDF in doc/.

Project vision
==============

I wrote mathblog with a very specific set of requirements in mind,
motivated by the following principles:

 - A blog should be easy to create, host, and update.

 - A blog should be easy to maintain.

 - I should be able to edit posts in my editor of choice and write
   them in an intelligent textual markup language.

 - It should be easy to embed high-quality mathematical symbols and
   equations in the blog posts.

As a result, mathblog has the following properties:

 - The software is composed of a single executable which will
   automatically take care of creating your blog and regenerating
   pages when your post markup changes.

 - All content is stored in plain text files and is generated
   statically.  No database or web framework is used.

 - A mathblog can be hosted with a simple static fileserver such as
   thttpd, Lighttpd, or Apache.

 - Blog posts are written in the Markdown format with extensions, as
   supported by the Pandoc document converter.

 - Math is embedded with `$...$` or `\(...\)` for inline math and
   `$$...$$` or `\[...\]` for block-level math.

These properties have some nice advantages; your blog content is
cacheable and can be subjected to revision control.  Posts are easy to
edit and editing doesn't require a web browser.  The static file
representation model means you can compose a blog post on your laptop
and get it just right using a local installation of mathblog, then
push it up to your server to post it to your public blog.

Dependencies
============

mathblog takes advantage of three primary software components:

 - Pandoc, a document-processing library.

 - Math typesetting packages:

   - MathJax if you choose `mathjax` for the value of the
     `mathBackend` configuration setting.  mathblog uses the MathJax
     CDN for MathJax resources.

 - Function graph plotting packages:

   - The TikZ and pgfplots LaTeX packages if you set `tikz = yes` in
     your config.  This is the recommended backend for function graph
     plotting.
