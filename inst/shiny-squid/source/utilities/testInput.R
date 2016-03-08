# test if the input is valid
testInput  <- function(input, inputConf, isInteger, errorOutput, modulo=FALSE){ 
    
  validInput <- TRUE
  
  if(!is.numeric(input) || (isInteger & !testInteger(input))){
    validInput <- FALSE
  }else{
    
    if(inputConf$min != "") if(input < inputConf$min) validInput <- FALSE
    if(inputConf$max != "") if(input > inputConf$max) validInput <- FALSE
  }
  
  if(modulo && validInput){
    if(inputConf$modulo %% input != 0) validInput <- FALSE
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

