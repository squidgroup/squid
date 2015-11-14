# create an icon into input label
info_msg <- function(infos){ 
  
  if(length(warnings)==0) return(NULL)
  
  infosOutput <- ""
  
  for(i in 1:length(infos)){
    infosOutput <- paste( infosOutput,
                           ifelse(i > 1,"</br>",""),
                           "<span class='glyphicon glyphicon-info-sign' aria-hidden='true'></span>
                            <span class='sr-only'>Warning:</span>",
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

