matrixInput2 <- function (inputId, label, data) 
{
  tagList(
    singleton(tags$head(tags$link(rel = "stylesheet", 
      type = "text/css", href = "css/tableinput.css"),
      tags$script(src = "js/tableinput.js"))
    ), 
  
    tags$div(class = "control-group tableinput-container", 
           
      tags$label(class = "control-label", label, tags$div(class = "tableinput-buttons")),
      
      tags$table(id = inputId, class = "tableinput data table table-bordered table-condensed", 
                 
       tags$colgroup(
         lapply(names(data), function(name) {
           tags$col(`data-name` = name, `data-field` = name,`data-type` = "numeric")
         })
       ),
        
        tags$thead(#class = "hide", 
                  tags$tr(lapply(names(data), function(name) { tags$th(name) }))
        ),
        
        tags$tbody(lapply(1:nrow(data), function(row) {            
            tags$tr(lapply(1:ncol(data), function(col) {
#             tags$td(div(contenteditable="true", as.character(data[row, col])))
              tags$td(div(as.character(data[row, col])))
              
            }))
        }))
      )
    )
  )
}
