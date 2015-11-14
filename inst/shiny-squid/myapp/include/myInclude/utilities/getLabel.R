
getLabel <- function(inputID, inputConf, placement="top", trigger="hover"){ 
  
  myInfoId <- paste(inputID,"Info",sep="_")
  
  return(span(strong(c(inputConf$label, getIcon(myInfoId))),
         bsTooltip(myInfoId, inputConf$infoTxt, placement, trigger)))

}