#Server functions for the full model
SVRGetModelEquation <- function(myModule, name, input, nb.IS){
 
  B               <- input[[paste(myModule, "B", sep="_")]] 
  Vind            <- input[[paste(myModule, "Vind", sep="_")]]
  X1_state        <- input[[paste(myModule, "X1_state", sep="_")]]
  X2_state        <- input[[paste(myModule, "X2_state", sep="_")]]
  X_Interaction   <- input[[paste(myModule, "X_Interaction", sep="_")]]
  VG              <- input[[paste(myModule, "VG_input", sep="_")]]
  Ve              <- input[[paste(myModule, "Ve_input", sep="_")]]
  
  myEqu <- paste(
    "$$",NOT[[paste("trait.",name,sep="")]],"_{",NOT$time,NOT$ind,NOT$group,"}=",
      # Interecept
      ifelse(B[1,nb.IS+B0]    != 0 || Vind[nb.IS+B0,nb.IS+B0] != 0, "(", ""),  
      ifelse(B[1,nb.IS+B0]    != 0, EQ[[paste("mean0.",name,sep="")]], ""),
      ifelse(B[1,nb.IS+B0]    != 0 & Vind[nb.IS+B0,nb.IS+B0] != 0, "+", ""),
      ifelse(Vind[nb.IS+B0,nb.IS+B0] != 0, EQ[[paste("dev0.",name,sep="")]], ""),
      ifelse(B[1,nb.IS+B0]    != 0 || Vind[nb.IS+B0,nb.IS+B0] != 0, ")+", ""), 
      
      # Slope 1 (X1)
      ifelse(X1_state != 0 & (B[1,nb.IS+X1] > 0 || Vind[nb.IS+X1,nb.IS+X1] > 0), "(", ""),
      ifelse(X1_state != 0 & B[1,nb.IS+X1] > 0, EQ[[paste("mean1.",name,sep="")]],""),
      ifelse(X1_state != 0 & B[1,nb.IS+X1] > 0 & Vind[nb.IS+X1,nb.IS+X1] > 0, "+",""),
      ifelse(X1_state != 0 & Vind[nb.IS+X1,nb.IS+X1] > 0, EQ[[paste("dev1.",name,sep="")]],""),
      ifelse(X1_state != 0 & (B[1,nb.IS+X1] > 0 || Vind[nb.IS+X1,nb.IS+X1] > 0), paste(")",EQ[["env1"]],"+", sep=""), ""),
      
      # Slope 2 (X2)
      ifelse(X2_state != 0 & (B[1,nb.IS+X2] > 0 || Vind[nb.IS+X2,nb.IS+X2] > 0), "(", ""),
      ifelse(X2_state != 0 & B[1,nb.IS+X2]    > 0, EQ[[paste("mean2.",name,sep="")]],""),
      ifelse(X2_state != 0 & B[1,nb.IS+X2]    > 0 & Vind[nb.IS+X2,nb.IS+X2] > 0, "+",""),
      ifelse(X2_state != 0 & Vind[nb.IS+X2,nb.IS+X2] > 0, EQ[[paste("dev2.",name,sep="")]],""),
      ifelse(X2_state != 0 & (B[1,nb.IS+X2] > 0 || Vind[nb.IS+X2,nb.IS+X2] > 0), paste(")",EQ[["env2"]],"+", sep=""), ""),  
      
      # Slope 3 (X1X2)
      ifelse(X_Interaction != 0 & (B[1,nb.IS+X1X2] > 0 || Vind[nb.IS+X1X2,nb.IS+X1X2] > 0), "(", ""),
      ifelse(X_Interaction != 0 & B[1,nb.IS+X1X2] > 0, EQ[[paste("mean12.",name,sep="")]],""),
      ifelse(X_Interaction != 0 & B[1,nb.IS+X1X2] > 0 & Vind[nb.IS+X1X2,nb.IS+X1X2] > 0, "+",""),
      ifelse(X_Interaction != 0 & Vind[nb.IS+X1X2,nb.IS+X1X2] > 0, EQ[[paste("dev12.",name,sep="")]],""),
      ifelse(X_Interaction != 0 & (B[1,nb.IS+X1X2] > 0 || Vind[nb.IS+X1X2,nb.IS+X1X2] > 0), paste(")",EQ[["env12"]],"+", sep=""), ""),
      
      ifelse(VG  > 0, paste(EQ[[paste("group.",name,sep="")]],"+",sep=""),""),
      ifelse(Ve  > 0, EQ[[paste("error.",name,sep="")]],""),
      "$$", sep="")
  
  if(substr(myEqu, nchar(myEqu)-2, nchar(myEqu)) == "+$$") myEqu <- paste(substr(myEqu, 1, nchar(myEqu)-3),"$$", sep="")
    
  
  return(withMathJax(myEqu))
  return(myEqu)
  
}