# Create checkbox input

getCheckboxInput<- function(inputID, inputConf, placement="top", trigger="hover") 
{    

  if(inputConf$infoTxt != ""){
    mylabel  <- c(inputConf$label,getIcon(inputConf$infoTxt, placement, trigger))
  }else{
    mylabel  <- inputConf$label
  }
	myOutput <-  span(checkboxInput(inputID, mylabel, value = inputConf$value))
  
  return(myOutput)
  
}
