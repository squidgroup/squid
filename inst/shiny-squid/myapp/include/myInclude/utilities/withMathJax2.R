withMathJax2 <- function(...){
  path <- "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))), 
          ..., tags$script(HTML("MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}
