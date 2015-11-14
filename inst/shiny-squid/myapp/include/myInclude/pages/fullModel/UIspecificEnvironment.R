UIspecificEnvironment <- function(Mod, name, type, isShared){
  
  return(
    span(
      switch(type, 
       # random environment with a normal distribution
       ran ={ 
         span(
            getCheckboxInput(paste(Mod,name,type,"state", sep="_"), FullModel_VAR[[type]][["state"]]),
            
            conditionalPanel(
              condition = paste("input.",Mod,"_",name,"_",type,"_state == 1", sep=""),                                                    
              
                fluidRow(
                  column(6,
                     getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$ranV, paste(Mod,"error",name,type,"V", sep="_"))
                   ),
                  column(6,
                    # Add decay rate (autocorrelation)
                    getCheckboxInput(paste(Mod,name,type,"autocorrelation", sep="_"), FullModel_VAR$ran_autocorrelation),
                    conditionalPanel(
                      condition = paste("input.",Mod,"_",name,"_",type,"_autocorrelation == 1", sep=""),
                      
                      getNumericInput(paste(Mod,name,type,"corr", sep="_"), 
                                     FullModel_VAR$ranCorr, 
                                     paste(Mod,"error",name,type,"corr",sep="_")) 
                    )
                  ),
                  column(12, getCheckboxInput(paste(Mod,name,type,"shared", sep="_"), FullModel_VAR[[type]][["share"]]))
              )
           )
         )
       }, 
       
       # Linear trend environment
       lin ={ 
         span(
           getCheckboxInput(paste(Mod,name,type,"state", sep="_"), FullModel_VAR[[type]][["state"]]),
           
           conditionalPanel(
             condition = paste("input.",Mod,"_",name,"_",type,"_state == 1", sep=""),                                                    
             
             fluidRow(
               column(6,
                getNumericInput(paste(Mod,name,type,"Intercept", sep="_"), FullModel_VAR$linI, paste(Mod,"error",name,type,"Intercept", sep="_"))
               ),
               column(6,
                getNumericInput(paste(Mod,name,type,"Slope", sep="_"), FullModel_VAR$linS, paste(Mod,"error",name,type,"Slope", sep="_"))
               ),
               column(12, getCheckboxInput(paste(Mod,name,type,"shared", sep="_"), FullModel_VAR[[type]][["share"]])),
               conditionalPanel(
                 condition = paste("input.",Mod,"_",name,"_",type,"_shared == 0", sep=""), 
                 getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$ranV, paste(Mod,"error",name,type,"V", sep="_"))
               )
             )
           )
         )
       }, 
       
       # Cyclic (Sinusoidal) environment (seasonal effect)
       cyc ={  
         span(
           getCheckboxInput(paste(Mod,name,type,"state", sep="_"), FullModel_VAR[[type]][["state"]]),  
           
           conditionalPanel(
             condition = paste("input.",Mod,"_",name,"_",type,"_state == 1", sep=""),    
             fluidRow(
               column(3,
                getNumericInput(paste(Mod,name,type,"Amplitude", sep="_"), FullModel_VAR$cycA, paste(Mod,"error",name,type,"Amplitude", sep="_"))
               ),
               column(3,
                getNumericInput(paste(Mod,name,type,"Period", sep="_"), FullModel_VAR$cycP, paste(Mod,"error",name,type,"Period", sep="_"))
               ),
               column(3,
                getNumericInput(paste(Mod,name,type,"Hshift", sep="_"), FullModel_VAR$cycH, paste(Mod,"error",name,type,"Hshift", sep="_"))
               ),
               column(3,
                getNumericInput(paste(Mod,name,type,"Vshift", sep="_"), FullModel_VAR$cycV, paste(Mod,"error",name,type,"Vshift", sep="_"))
               ),
               column(12, getCheckboxInput(paste(Mod,name,type,"shared", sep="_"), FullModel_VAR[[type]][["share"]])),
               conditionalPanel(
                 condition = paste("input.",Mod,"_",name,"_",type,"_shared == 0", sep=""), 
                 getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$ranV, paste(Mod,"error",name,type,"V", sep="_"))
               )
             )
           )
         )
       },                   
       
       {print('Default')} # Default
      ) # End switch
    )# End span
  ) # End return
}