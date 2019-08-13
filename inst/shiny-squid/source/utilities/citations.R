# functions for references and citations
print_ref <- function(bib, .opts){
  
  out_bib <- lapply(1:length(bib), function(i){
    
    bib_authors <- bib[i]$author
    
      # Authors
    out_authors <- lapply(1:length(bib_authors), function(j){
        paste(bib_authors[j]$family,                  # family name
               paste0(toupper(substring(bib_authors[j]$given, 1, 1)), ".", collapse = " "), # given name
               sep = ", ")  
    })
    
    if(length(out_authors) > 1){
      out_authors <- paste(paste0(out_authors[1:(length(out_authors)-1)], collapse = ", "), out_authors[length(out_authors)], sep="& ")
    }
    
  paste0("<p>", out_authors, " (", bib[i]$year, ") ", bib[i]$title, ". "
              , "<i>", bib[i]$journal, "</i>. <b>", bib[i]$volume, "</b>, ", bib[i]$pages, ". "
              , "<a href='", bib[i]$url,"' target='_blank'>DOI: ", bib[i]$doi, "</a>.",
    "</p>")
    
  })
  
  return(HTML(paste0(out_bib)))

}