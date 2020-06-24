# test if the input is valid
testInputBMatrix  <- function(input, inputConf, errorOutput){ 
  
  validInput <- TRUE
  
  if(!is.numeric(input) || any(is.na(input))){
    validInput <- FALSE
  }else{
    if(inputConf$min != "") if(any(input < inputConf$min)) validInput <- FALSE
    if(inputConf$max != "") if(any(input > inputConf$max)) validInput <- FALSE
  }
  
  if(errorOutput){
   if(!validInput){
    return(error_msg(inputConf$errorTxt))
   }else{
     return(NULL)
   }
  }
  
  return(validInput)  
}

