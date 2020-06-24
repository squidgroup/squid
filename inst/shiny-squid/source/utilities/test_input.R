# test if the input is valid
testInput  <- function(input, inputConf, isInteger, errorOutput, extraCondition=FALSE){ 
    
  validInput <- TRUE
  
  if(!is.numeric(input) || (isInteger & !testInteger(input)) || extraCondition){
    validInput <- FALSE
  }else{
    
    if(inputConf$min != "") if(input < inputConf$min) validInput <- FALSE
    if(inputConf$max != "") if(input > inputConf$max) validInput <- FALSE
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

