getSelectInput <- function(inputID, inputConf, placement="top", trigger="hover") 
{    
  
  if(inputConf$infoTxt != ""){
    myInfoId <- paste(inputID,"Info",sep="_")
    mylabel  <- c(inputConf$label,getIcon(myInfoId))
    myOutput <-  span(
                    selectInput(inputID, 
                                mylabel, 
                                inputConf$value),
                    bsTooltip(myInfoId, inputConf$infoTxt, placement, trigger)
                )
  }else{
    mylabel <- inputConf$label
    myOutput <-  span(
                    selectInput(inputID, 
                                mylabel, 
                                inputConf$value)
                )
  }
  
  return(myOutput)
}

