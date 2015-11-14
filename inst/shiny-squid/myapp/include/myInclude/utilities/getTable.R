# test if the number is an integer
getTable  <- function(myTable){ 
  
  myHTMLTable <- tags$table(class = "table table-striped table-hover dataTable", 
                            tags$thead(
                              tags$tr(lapply(names(myTable), function(name) { tags$th(name) }))
                            ),
                            tags$tbody(lapply(1:nrow(myTable), function(row) {            
                              tags$tr(lapply(1:ncol(myTable), function(col) {
                                tags$td(withMathJax(myTable[row, col]))
                              }))
                            }))
                  )
  
  return(myHTMLTable)
  
}

