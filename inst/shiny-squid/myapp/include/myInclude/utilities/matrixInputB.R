matrixInputB <- function (inputId, label, data, dataOn, NT, NTnames, nb.IS) 
{
  tagList(
    singleton(tags$head(tags$link(rel = "stylesheet", 
      type = "text/css", href = "css/tableinput.css")
      ,
      tags$script(src = "js/tableinput.js")
)
    ), 
  
    tags$div(class = "control-group tableinput-container", 
           
#       tags$label(class = "control-label", label, tags$div(class = "tableinput-buttons")), 
      
      tags$table(id = inputId, class = "tableinput data table table-bordered table-condensed", 
                 
        tags$colgroup(
          lapply(names(data), function(name) {
            tags$col(`data-name` = name, `data-field` = name,`data-type` = "numeric")
          })
        ), 
        
        tags$thead( 
          # tags$tr(lapply(1:NT, function(trait) { tags$th(paste("Trait",NTnames[trait]), class="table-header",colspan=nb.IS) })),
          tags$tr(lapply(names(data), function(name) { withMathJax(tags$th(HTML(name))) }))
        ),
        
        tags$tbody(lapply(1:nrow(data), function(row) {            
            tags$tr(lapply(1:ncol(data), function(col) {
              
              cellColor <- ifelse(dataOn[row, col], "BMat_input","BMat_noInput") 
              cellHide  <- ifelse(dataOn[row, col], "","invisible")
              
              tags$td(class=cellColor,div(class = cellHide, contenteditable="true", as.character(data[row, col])))
            }))
        }))
      )
    )
  )
}
