# Create numeric input

getNumericInput<- function(inputID, inputConf, errorID, placement="top", trigger="hover") 
{    

  if(inputConf$infoTxt != ""){
    myInfoId <- paste(inputID,"Info",sep="_")
    mylabel  <- c(inputConf$label,getIcon(myInfoId))
    myOutput <-  span(
      numericInput(inputID,
                   mylabel, 
                   inputConf$value, 
                   min  = inputConf$min, 
                   max  = inputConf$max, 
                   step = inputConf$step),
      uiOutput(errorID),
      bsTooltip(myInfoId, inputConf$infoTxt, placement, trigger)
    )
  }else{
    mylabel <- inputConf$label
    myOutput <- span(
                  numericInput(inputID,
                               mylabel, 
                               inputConf$value, 
                               min  = inputConf$min, 
                               max  = inputConf$max, 
                               step = inputConf$step),
                  uiOutput(errorID)
    )
  }
  
  return(myOutput)
  
}
