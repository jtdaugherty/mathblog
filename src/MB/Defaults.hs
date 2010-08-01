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
               \        <a id=\"listing\" href=\"/list.html\">all posts</a>\n\
               \      </div>\n"

pagePostamble :: String
pagePostamble = "    </div>\n\
                \  </body>\n\
                \</html>"

postPreamble :: String
postPreamble = "<div class=\"post\">"

postPostamble :: String
postPostamble = "</div>\n\
                \<div id=\"footer\">\n\
                \  Copyright &copy; 2010 Your Name Here\n\
                \</div>"

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
             \h3 {\n\
             \  font-weight: normal;\n\
             \}\n\
             \#header a {\n\
             \  color: #00f;\n\
             \  text-decoration: none;\n\
             \}\n\
             \a#listing {\n\
             \  float: right;\n\
             \  font-size: 14px;\n\
             \  margin-top: 1em;\n\
             \}\n\
             \div#all-posts {\n\
             \  margin-top: 1.5em;\n\
             \}\n\
             \div.listing-entry {\n\
             \  border-bottom: 1px solid #aaa;\n\
             \  margin-top: 0.5em;\n\
             \  padding-bottom: 0.5em;\n\
             \}\n\
             \span.post-title {\n\
             \  font-size: 20px;\n\
             \  display: block;\n\
             \}\n\
             \span.post-title a {\n\
             \  text-decoration: none;\n\
             \  color: blue;\n\
             \}\n\
             \span.post-created {\n\
             \  font-size: 15px;\n\
             \  display: block;\n\
             \  margin-left: 1em;\n\
             \  color: #555;\n\
             \}\n\
             \div#prev-next-links {\n\
             \  margin-top: 1em;\n\
             \}\n\
             \span.subdued {\n\
             \  color: #aaa;\n\
             \}\n\
             \.next-link {\n\
             \  float: right;\n\
             \  color: blue;\n\
             \  text-decoration: none;\n\
             \}\n\
             \.prev-link {\n\
             \  color: blue;\n\
             \  text-decoration: none;\n\
             \}\n\
             \#footer {\n\
             \  border-top: 1px solid #ddd;\n\
             \  padding-top: 0.5em;\n\
             \  color: #777;\n\
             \  font-size: smaller;\n\
             \  text-align: right;\n\
             \}\n\
             \"

firstPost :: String
firstPost = "%A first post on the nature of $R$\n\
            \\n\
            \This is an initial post which you should delete.  This is just\n\
            \here to show how the system works.  Inline math looks like $x \\in Y$\n\
            \and:\n\
            \\n\
            \$\\int^n_0{x^2 dx} = \\sum^n_{i=1}{i^n}$\n\
            \\n\
            \$$\\int^n_0{x^2 dx} = \\sum^n_{i=1}{i^n}$$\n\
            \\n\
            \Summary\n\
            \-------\n\
            \\n\
            \$$\\int^n_0{i^2}$$\n\
            \\n\
            \Posts are written in extended Markdown as described in the <a href=\"http://johnmacfarlane.net/pandoc/README.html#pandocs-markdown-vs.standard-markdown\">\
            \Pandoc User Guide</a>.  Embedded math is rendered with <a href=\"http://ans.hsh.no/home/mgg/gladtex/\">gladTeX</a>.\n\
            \"
