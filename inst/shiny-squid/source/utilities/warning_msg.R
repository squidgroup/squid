# create a warning message
warning_msg <- function(warning){ 
  
  if(length(warning)==0) return(NULL)
  
  warningsOutput <- ""
  
  for(i in 1:length(warning)){
    warningsOutput <- paste( warningsOutput,
                          ifelse(i > 1,"</br>",""),
                          "<span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>
                            <span class='sr-only'>Warning!</span>",
                          warning[i],"")
  }
  
  return(
    HTML(
      paste("<div class='alert alert-warning' role='alert'>",
            warningsOutput,
            "</div>")
    )
  )
}

