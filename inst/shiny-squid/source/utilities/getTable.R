# test if the number is an integer
getTable  <- function(myTable, header=FALSE){ 
  
  if(header){
    tableNames <- myTable[1,]
    start      <- 2  
  }else{
    tableNames <- names(myTable)
    start      <- 1   
  }
  
  myHTMLTable <- tags$table(class = "table table-striped table-hover dataTable", 
                            tags$thead(
                              tags$tr(lapply(tableNames, function(name) { tags$th(name) }))
                            ),
                            tags$tbody(lapply(start:nrow(myTable), function(row) {            
                              tags$tr(lapply(1:ncol(myTable), function(col) {
                                tags$td(myTable[row, col])
                              }))
                            }))
  )
  return(withMathJax(myHTMLTable))
}

