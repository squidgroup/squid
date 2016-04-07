
getLabel <- function(inputID, inputConf, placement="top", trigger="hover"){ 

  return(span(strong(c(inputConf$label, getIcon(inputConf$infoTxt, placement, trigger)))))
}