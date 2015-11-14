# Create numer input

getCheckboxInput<- function(inputID, inputConf, placement="top", trigger="hover") 
{    

  if(inputConf$infoTxt != ""){
    myInfoId <- paste(inputID,"Info",sep="_")
    mylabel  <- c(inputConf$label,getIcon(myInfoId))
    myOutput <-  span(
      checkboxInput(inputID, mylabel, value = inputConf$value),
      bsTooltip(myInfoId, inputConf$infoTxt, placement, trigger)
    )
  }else{
    mylabel  <- inputConf$label
    myOutput <-  span(checkboxInput(inputID, mylabel, value = inputConf$value))
  }
  
  return(myOutput)
  
}
