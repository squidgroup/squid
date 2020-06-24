# test if the input is valid
testInputVindMatrix  <- function(input, inputConf, errorOutput){ 
  
  validInputDiag <- TRUE
  validInput     <- TRUE
  errorTxt       <- NULL 
  myDiag         <- diag(input)
  
  
  if(!is.numeric(myDiag) || any(is.na(myDiag))){
    validInputDiag <- FALSE
  }else{
    
    if(inputConf$diagmin != "") if(any(myDiag < inputConf$diagmin)) validInputDiag <- FALSE
    if(inputConf$diagmax != "") if(any(myDiag > inputConf$diagmax)) validInputDiag <- FALSE
  }
  
  if(!validInputDiag) errorTxt <- c(errorTxt, inputConf$errorTxt1)
  
  diag(input) <- 0 
  
  if(!is.numeric(input) || any(is.na(input))){
    validInput <- FALSE
  }else{

    if(inputConf$min != "") if(any(input < inputConf$min)) validInput <- FALSE
    if(inputConf$max != "") if(any(input > inputConf$max)) validInput <- FALSE
  }
  
  if(!validInput) errorTxt <- c(errorTxt, inputConf$errorTxt2)
  
  if(errorOutput){
   if(!validInput || !validInputDiag){
    return(error_msg(errorTxt))
   }else{
     return(NULL)
   }
  }
  
  return(validInput*validInputDiag)  
}

