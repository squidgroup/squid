# create an info message
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

