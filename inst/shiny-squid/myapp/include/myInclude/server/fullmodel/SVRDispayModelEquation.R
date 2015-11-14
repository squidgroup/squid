#Server functions for the full model
SVRDispayModelEquation <- function(myModule, input){
   
   NT         <- input[[paste(myModule, "_NT", sep="")]]
   B          <- input[[paste(myModule, "_B", sep="")]]
  
   myEqu1 <- SVRGetModelEquation(myModule, "1", input, 0)
   
   if(NT < 2 || length(B) == nb.IS){
     return(list(myEqu1))
   }else{     
     myEqu2 <- SVRGetModelEquation(myModule, "2", input, nb.IS)     
     return(list(myEqu1, myEqu2))       
   } 

}