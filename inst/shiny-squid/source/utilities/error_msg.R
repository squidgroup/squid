# create an error message
error_msg <- function(errors){ 
  
  if(length(errors)==0) return(NULL)
  
  errorsOutput <- ""
  
  for(i in 1:length(errors)){
    errorsOutput <- paste( errorsOutput,
                           ifelse(i > 1,"</br>",""),
                           "<span class='glyphicon glyphicon-exclamation-sign' aria-hidden='true'></span>
                            <span class='sr-only'>Error!</span>",
                           errors[i],"")
  }
    
  return(
        HTML(
          paste("<div class='alert alert-danger' role='alert'>",
                errorsOutput,
                "</div>")
        )
    )
}

