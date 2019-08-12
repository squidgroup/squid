# functions for references and citations

print_ref <- function(bib, .opts){return(HTML(capture.output(print(bib, .opts = .opts))))}