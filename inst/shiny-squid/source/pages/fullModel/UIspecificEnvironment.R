UIspecificEnvironment <- function(Mod, name, type, isShared){
  
  return(
    span(
      switch(type, 
       # Stochastic environment with a normal distribution
       sto ={ 
         span(
            getCheckboxInput(paste(Mod,name,type,"state", sep="_"), FullModel_VAR[[type]][["state"]]),
            
            conditionalPanel(
              condition = paste("input.",Mod,"_",name,"_",type,"_state == 1", sep=""),                                                    
              
                fluidRow(
                  column(6,
                     getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$stoV, paste(Mod,"error",name,type,"V", sep="_"))
                   ),
                  column(6,
                    # Add decay rate (autocorrelation)
                    getCheckboxInput(paste(Mod,name,type,"autocor_state", sep="_"), FullModel_VAR$sto_autocor_state),
                    conditionalPanel(
                      condition = paste("input.",Mod,"_",name,"_",type,"_autocor_state == 1", sep=""),
                      
                      getNumericInput(paste(Mod,name,type,"corr", sep="_"), 
                                     FullModel_VAR$stoCorr, 
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
                getNumericInput(paste(Mod,name,type,"intercept", sep="_"), FullModel_VAR$linI, paste(Mod,"error",name,type,"intercept", sep="_"))
               ),
               column(6,
                getNumericInput(paste(Mod,name,type,"slope", sep="_"), FullModel_VAR$linS, paste(Mod,"error",name,type,"slope", sep="_"))
               ),
               column(12, getCheckboxInput(paste(Mod,name,type,"shared", sep="_"), FullModel_VAR[[type]][["share"]])),
               conditionalPanel(
                 condition = paste("input.",Mod,"_",name,"_",type,"_shared == 0", sep=""), 
                 getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$stoV, paste(Mod,"error",name,type,"V", sep="_"))
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
                getNumericInput(paste(Mod,name,type,"amplitude", sep="_"), FullModel_VAR$cycA, paste(Mod,"error",name,type,"amplitude", sep="_"))
               ),
               column(3,
                getNumericInput(paste(Mod,name,type,"period", sep="_"), FullModel_VAR$cycP, paste(Mod,"error",name,type,"period", sep="_"))
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
                 getNumericInput(paste(Mod,name,type,"V", sep="_"), FullModel_VAR$stoV, paste(Mod,"error",name,type,"V", sep="_"))
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