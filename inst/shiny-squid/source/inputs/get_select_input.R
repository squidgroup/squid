# Create a select input

getSelectInput <- function(inputID, inputConf, placement="top", trigger="hover") 
{    
  
  if(inputConf$infoTxt != ""){
  	mylabel  <- c(inputConf$label,getIcon(inputConf$infoTxt, placement, trigger))
  }else{
    mylabel <- inputConf$label
  }
	
	myOutput <-  span(
		selectInput(inputID, 
								mylabel, 
								inputConf$value)
	)
	
	return(myOutput)
}

