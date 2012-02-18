
mathblog
--------

mathblog is a Haskell program targeted at people who want to write
statically-generated, mathematically-themed weblogs.  It supports:

 - Extended Markdown input syntax as supported by the Pandoc library

 - inline and block-level TeX math rendered by MathJax or LaTeX

 - Function graphing with Gnuplot or the TikZ / pgfplots LaTeX
   packages

 - Integration of Javascript-based web services such as Disqus

 - Template-based document rendering with support for layout and style
   customization

Project vision
--------------

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
------------

mathblog takes advantage of three primary software components:

 - Pandoc, a document-processing library.

 - Math typesetting packages:

   - GladTex and LaTeX for math typesetting if you choose `gladtex`
     for the value of the 'mathBackend' configuration setting.
     GladTex 1.3 is required for best results.  GladTex renders LaTeX
     snippets to images.  This is the recommended backend for math
     typesetting.  See <https://sourceforge.net/projects/gladtex/>

   - MathJax if you choose `mathjax` for the value of the
     'mathBackend' configuration setting.  mathblog uses the MathJax
     CDN for MathJax resources.

 - Function graph plotting packages:

   - The TikZ and pgfplots LaTeX packages if you choose `gnuplot` for
     the value of the 'eqBackend' configuration setting.  This is the
     recommended backend for function graph plotting.

   - GNUplot if you choose `gnuplot` for the value of the 'eqBackend'
     configuration setting.

Creating a blog
---------------

To begin, set the following environment variable:

  `MB_BASE_DIR`

This is the location of your blog files on the filesystem.  It must be
an absolute path.  If you would rather use a command-line parameter,
specify `-d` or `--baseDir` when running `mb`.  The command line
directory parameter will always override the environment variable if
it is set.

Once you've set the environment variable, just run `mb -i`.  It will
take care of setting up a new blog data directory for you, complete
with some default pages and a default first blog post:

~~~
$ export MB_BASE_DIR=`pwd`/myBlog
$ mb -i
Blog directory: .../myBlog
Setting up data directory using skeleton: /.../skel
Configuration file changed; regenerating all content.
Templates changed; regenerating accordingly.
Posts changed:
  first-post.txt
Post index changed; regenerating next/previous links.
Rendering first-post
Done.
~~~

Configuring the Blog
--------------------

The default blog configuration file is called `blog.cfg`.  This file
contains information about you which may be included in the generated
pages.  The configuration file `blog.cfg` must have the following
fields set:

 - `baseUrl`

    The base URL of your blog; this URL will be used to generate some
    links in the blog's pages where absolute URLs matter.  Otherwise,
    relative URLs will be used, with an assumption that your blog is
    hosted at the root of a domain.

 - `title`

    The title of your blog, such as "My math blog".

 - `authorName`

    Your name (for the RSS feed metadata and the page footer).

 - `authorEmail`

    Your e-mail address (for the RSS feed metadata only).

 - `mathBackend`

    The backend used to render TeX math expressions.  Can be either
    `gladtex` or `mathjax`.  If `gladtex`, you will need the latest
    version of the `gladtex` program for best results.  No special
    configuration is necessary for MathJax, as mathblog's default
    templates use CDN resources for MathJax.

 - `eqBackend`

    The backend used to render function graphs.  Can be either `tikz`
    or `gnuplot`.  See the Function Graph Embedding section below for
    details.

Note that if you pick a math or function graphing backend which isn't
supported by your system, `mb` will emit errors when processing your
posts.

All of the above fields can be accessed in templates using the syntax
described in "Customizing your blog" below.

Serving the Blog
----------------

To serve your blog files you can either make the "html" directory a
document root for your web server or virtual host, or you can symlink
the `html` directory to the location of choice.  The mathblog
directory itself is not intended to be in your web server's document
tree because it contains many files that shouldn't be served to users.

Start up a web server pointing at the `html` directory and take a
look.

Now you might want to edit or create a new post, or even remove one.
Just edit the appropriate file in `posts/` accordingly and re-run
`mb`.  Here's an example of adding a new post:

~~~
$ mb
mb: using base directory ".../myBlog"
Posts changed:
  first-post.txt
Rendering 1 post(s)...
Rendering first-post
Done.
~~~

When to run `mb`
----------------

`mb` looks at the modification times of your post files in `posts/`,
the config file, template files, and the post index (see below) when
determining when to regenerate content.  It also looks for new posts
that haven't been rendered in the past.  The rule of thumb is: re-run
mb whenever you make any changes to:

 - templates in `templates/`
 - post files in `posts/`
 - the post index, `posts/posts-index`
 - `blog.cfg`

Modifying a post will cause that post to be re-rendered, but
modifiying the configuration file will cause ALL posts to be
re-rendered (since the configuration file contains values that will
affect all hyperlinks contained inside each post).  `mb` will always
rebuild the portions of each page affected by template changes even if
post source files are unchanged.

Post format
-----------

Posts are formatted in Markdown and support the extended Markdown
syntax as implemented by Pandoc.  The only important convention to
note is that the post title goes on the first line of the file
following a percent sign, as follows:

~~~
%My First Post

First paragraph starts here.
~~~

Running mb in "listen" mode
---------------------------

Ordinarily, you'll just run `mb` once in a while to update your
content.  But if you're in the middle of authoring a new post and you
want to see what it looks like as you edit it rather than run `mb` by
hand periodically, you can run `mb` in "listen" mode; this means that
mb will run forever, peroidically scanning the filesystem for changes
to your blog posts and other data files and regenerate them when it
detects a change.  Running mathblog in listen mode is simple:

~~~
$ mb -l
Blog directory: /.../blog
Post index changed; regenerating next/previous links.
Done.

Blog directory: /.../blog
Configuration file changed; regenerating all content.
Rendering 1 post(s)...
Rendering first-post
Done.

Blog directory: /Users/cygnus/blog3
Posts changed:
  first-post.txt
Rendering 1 post(s)...
Rendering first-post
Done.
~~~

Above I ran `mb` in listen mode and then modified the post index, then
the blog config file, then one of the posts.  In each case `mb`
detected the change and ran its typical regeneration routine.

Customizing your blog
---------------------

It's likely that you'll want to customize the look and feel of your
blog.  To this end, mathblog generates the pages of your blog by
assembling various pieces of the page to create the final result.  The
biggest piece of a generated page is the blog post itself, but the
surrounding elements are read from various files that are created by
mathblog when it creates your blog data directory.  These files are
stored in the `templates/` subdirectory of your blog data directory
and are as follows:

 - `templates/rssTemplate.xml`

    This is the template used to generate your RSS feed.

 - `templates/pageTemplate.html`

    This file makes up the overall structure of every page on the
    blog.

 - `templates/postTemplate.html`

    This file makes up the structure of the post portion of the page,
    for pages which show posts (i.e., not the post index).

In addition, subdirectories of the `html/` directory contain things
you might want to customize, such as a CSS stylesheet.

The templates mentioned above are StringTemplate templates and are
processed with the HStringTemplate library.  The following template
placeholders are supported in each template:

  - `$title$`, `$baseUrl$`, `$authorName$`, `$authorEmail$`

    These placeholders all correspond directly to fields on the
    `blog.cfg` configuration file.

  - `$extraPageHead$`

    Content to be placed in the `<HEAD>` tag of the page, such as
    javascript tags, stylesheets, etc.  You'll need to ensure that
    this is somewhere in your `<HEAD>` tag if you want to use mathblog
    features which may need to load extra resources.

These placeholders are supported in the post template:

 - `$jsInfo$`

    A brief javascript used to provide information about the page to
    other javascripts (see "Other features" below for usage).

 - `$nextPrevLinks$`

    HTML displaying the "next" and "previous" links for older/newer
    posts.

 - `$post$`

    The body of the post itself.

These placeholders are supported in the page template:

 - `$content$`

    The content of the page to be rendered.

Function Graph Embedding
------------------------

mathblog supports inline scripts for rendering function graphs.  Right
now, mathblog supports Gnuplot and the TikZ / pgfplots LaTeX packages.
Set the 'eqBackend' configuration option (see above) to choose a
backend.

Gnuplot
=======

To specify a Gnuplot function graph in a blog post, we overload the
Pandoc code block syntax.  Here's an example of a Gnuplot graph:

    ~~~ {#eq-basic}
    f(x) = x
    plot [-5.0:5.0] [-5.0:5.0] f(x) lt rgb "blue"
    ~~~

This defines a single function and plots it in blue over the specified
intervals.  The most important part is the "#eq-basic" bit: this
specifies the equation preamble to use when generating the output
image.  This string refers to this preamble file in your blog
directory:

  `eq-preambles/eq-basic.txt`

This file contains the commands responsible for determining the output
image size and axis configuration.  The contents of the code block
that you write get appended to the contents of the preamble file you
specify to result in the full gnuplot script, whose output gets
embedded in the page.  We use these templates to make it easier to
create many graphs that have the same general structure (e.g., axis
configuration, image size) without having to repeat the full gnuplot
script each time.

You can create other gnuplot premables to suit your needs; just
reference them in the code block in the same way.  For example, you
might create eq-preambles/eq-fancy.txt, in which case the code block
syntax to use it is:

    ~~~ {#eq-fancy}
    <your script here>
    ~~~

TikZ / pgfplots
===============

To specify a TikZ / pgfplots function graph in a blog post, we
overload the Pandoc code block syntax.  Here's an example of a TikZ
figure:

    ~~~ {#tikz}
    \begin{axis}[
            minor tick num=3,
            axis y line=center,
            axis x line=middle,
        ]

    \addplot[smooth,mark=none,blue] plot coordinates {
        (-1,-1)
        (2.95,2.95)
    };
    ~~~

This is a LaTeX fragment which will automatically be embedded in a
`tikzpicture` environment and rendered to an image within the blog post.  For more information on the powerful TikZ / pgfplots packages, please see

  <https://sourceforge.net/projects/pgfplots/>

Styling Function Graphs with CSS
================================

Note that the Pandoc syntax also allows us to assign CSS class names
to the code block, and mathblog passes these through to the generated
image.  So if you wanted to wrap your text around the generated image,
you could create a CSS class like this:

    .eq-right {
      float: right;
    }

and then assign it to your equation graph like this:

    ~~~ {#eq-basic .eq-right}
    ...
    ~~~

For more information on the code block syntax, please see:

  <http://johnmacfarlane.net/pandoc/README.html#delimited-code-blocks>

Other features
--------------

Since mathblog doesn't provide many moving parts, it's up to you to
outsource various web site features, such as comments.  I've
successfully integrated mathblog with the Disqus comments service.  To
do this, some javascript needs to be embedded in the blog pages.
Disqus works best when you supply it with a page identifier so it can
guarantee that comments are post-specific rather than URL-specific.
The way mathblog makes this possible is by exposing a JavaScript
variable to other scripts in your page:

    Blog.pageName = "foobar-baz";

This variable name can be used by scripts you embed, such as with
Disqus comment forms:

    var disqus_identifier = Blog.pageName;

Controlling Post Order
----------------------

Whenever you add a new post, mb automatically updates the "post
index", a file called `posts-index` in the post source directory.
This file lists the filenames of all posts from newest to oldest.  By
default, new posts get added to the beginning of the list, as you
would expect.  Any new posts added to the list are sorted by
modification time so that the newest post on disk appears earlier in
the index.  This feature exists to make it possible for older posts to
be updated without changing their ordering in the overall sequence of
posts.

At any time, you can edit the index to reorder the posts as you see
fit.  `mb` will preserve ordering of posts already in the index when
you run it in the future.

`mb` will also take care of removing posts from the index if they've
been removed from the post source directory.

Afterword
---------

I personally use this software package but I'll be pleased if others
find it useful.  In addition, I'm open to accepting contributions on
the project if they're consistent with what I've outlined above.
Happy blogging!

Jonathan Daugherty (<drcygnus@gmail.com>)