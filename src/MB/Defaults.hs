module MB.Defaults
    ( preamble
    , postamble
    , stylesheet
    , firstPost
    )
where

preamble :: String
preamble = "<html>\n\
           \  <head>\n\
           \    <title>A Blog</title>\n\
           \    <link rel=\"stylesheet\" type=\"text/css\" href=\"/stylesheets/stylesheet.css\"/>\n\
           \  </head>\n\
           \  <body>\n\
           \    <div id=\"header\">\n\
           \      <a href=\"/\">A Blog</a>\n\
           \    </div>\n"

postamble :: String
postamble = "  </body>\n\
            \</html>"

stylesheet :: String
stylesheet = "body {\n\
             \  margin-left: auto;\n\
             \  margin-right: auto;\n\
             \  width: 60em;\n\
             \}"

firstPost :: String
firstPost = ""