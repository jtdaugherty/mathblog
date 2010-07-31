module MB.Defaults
    ( pagePreamble
    , pagePostamble
    , postPreamble
    , postPostamble
    , stylesheet
    , firstPost
    )
where

pagePreamble :: String
pagePreamble = "<html>\n\
               \  <head>\n\
               \    <title>A Blog</title>\n\
               \    <link rel=\"stylesheet\" type=\"text/css\" href=\"/stylesheets/stylesheet.css\"/>\n\
               \  </head>\n\
               \  <body>\n\
               \    <div id=\"page\">\n\
               \      <div id=\"header\">\n\
               \        <a href=\"/\">A Blog</a>\n\
               \        <a id=\"listing\" href=\"/list.html\">previous posts</a>\n\
               \      </div>\n"

pagePostamble :: String
pagePostamble = "    </div>\n\
                \  </body>\n\
                \</html>"

postPreamble :: String
postPreamble = "<div class=\"post\">"

postPostamble :: String
postPostamble = "</div>"

stylesheet :: String
stylesheet = "html {\n\
             \  background: #aaa;\n\
             \}\n\
             \body {\n\
             \  font-size: 16px;\n\
             \  margin-left: 3em;\n\
             \  margin-right: 3em;\n\
             \  background: #fff;\n\
             \  margin-top: 0px;\n\
             \  min-height: 900px;\n\
             \}\n\
             \#page {\n\
             \  padding-top: 1em;\n\
             \  padding-left: 1em;\n\
             \  padding-right: 1em;\n\
             \  margin-left: auto;\n\
             \  margin-right: auto;\n\
             \  max-width: 700px;\n\
             \}\n\
             \#header {\n\
             \  font-size: 26px;\n\
             \}\n\
             \h1 {\n\
             \  font-size: 32px;\n\
             \  font-weight: normal;\n\
             \}\n\
             \#header a {\n\
             \  color: #00f;\n\
             \  text-decoration: none;\n\
             \}"

firstPost :: String
firstPost = "%A first post on the nature of $R$\n\
            \\n\
            \This is an initial post which you should delete.  This is just\n\
            \here to show how the system works.  Inline math looks like $x \\in Y$\n\
            \and:\n\
            \\n\
            \$\\int^n_0{x^2 dx} = \\sum^n_{i=1}{i^n}$\n\
            \\n\
            \Summary\n\
            \-------\n\
            \\n\
            \Posts are written in extended Markdown as described in the <a href=\"http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown-vs.standard-markdown\">\
            \Pandoc User Guide</a>.  Embedded math is rendered with <a href=\"http://ans.hsh.no/home/mgg/gladtex/\">gladTeX</a>.\n\
            \"
