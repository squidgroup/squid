getSliderInput<- function(inputID, inputConf, placement="top", trigger="hover") 
{    
  
  if(inputConf$infoTxt != ""){
    myInfoId <- paste(inputID,"Info",sep="_")
    mylabel  <- c(inputConf$label,getIcon(myInfoId))
    myOutput <-  span(
                    sliderInput(inputID,
                                mylabel, 
                                value = inputConf$value, 
                                min   = inputConf$min, 
                                max   = inputConf$max, 
                                step  = inputConf$step,
                                width = "500px",
                                post = ""
                    ),
                    bsTooltip(myInfoId, inputConf$infoTxt, placement, trigger)
                )
  }else{
    mylabel <- inputConf$label
    myOutput <-  span(
                  sliderInput(inputID,
                              mylabel, 
                              value = inputConf$value, 
                              min   = inputConf$min, 
                              max   = inputConf$max, 
                              step  = inputConf$step,
                              width = "500px",
                              post = ""
                  )
                )
  }
  
  return(myOutput)
}

