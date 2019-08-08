

###### create an info message ###### 
info_msg <- function(infos){ 
  
  if(length(infos)==0) return(NULL)
  
  infosOutput <- ""
  
  for(i in 1:length(infos)){
    infosOutput <- paste( infosOutput,
                          ifelse(i > 1,"</br>",""),
                          "<span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>
                            <span class='sr-only'>Infos!</span>",
                          infos[i],"")
  }
  
  return(
    HTML(
      paste("<div class='alert alert-info' role='alert'>",
            infosOutput,
            "</div>")
    )
  )
}

###### create an info message ###### 
warning_msg <- function(infos){ 
  
  if(length(infos)==0) return(NULL)
  
  infosOutput <- ""
  
  for(i in 1:length(infos)){
    infosOutput <- paste( infosOutput,
                          ifelse(i > 1,"</br>",""),
                          "<span class='glyphicon glyphicon-warning-sign' aria-hidden='true'></span>
                            <span class='sr-only'>Warning!</span>",
                          infos[i],"")
  }
  
  return(
    HTML(
      paste("<div class='alert alert-warning' role='alert'>",
            infosOutput,
            "</div>")
    )
  )
}


###### create an error message ###### 
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


# run simulation message
sim_msg <- function(){return(warning_msg('Please click on the RUN or REFRESH button to see or refresh results.'))}