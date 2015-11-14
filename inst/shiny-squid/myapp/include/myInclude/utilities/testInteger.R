# test if the number is an integer
testInteger  <- function(x){ 
  
  test <- all.equal(x, as.integer(x), check.attributes = FALSE)
  
  if(test == TRUE){ 
    return(TRUE) 
  }else{ 
    return(FALSE) 
  }
}

