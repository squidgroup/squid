UIenvironment <- function(Mod, name, isShared){

  return(
    span(
      fluidRow(
         column(6,
          wellPanel( 
             UIspecificEnvironment(Mod, name, "sto", isShared)
           )
         ),                 
         column(6,
           wellPanel( 
              UIspecificEnvironment(Mod, name, "lin", isShared)
           )
         )
      ),
      
      fluidRow(
        column(12,
          wellPanel( 
            UIspecificEnvironment(Mod, name, "cyc", isShared)
          )
        )
      ),
      
      conditionalPanel(
        condition = paste("input.",Mod,"_",name,"_sto_state == 1 || input.",
                          Mod,"_",name,"_lin_state == 1 || input.",
                          Mod,"_",name,"_cyc_state == 1", sep=""),
					
          div(info_msg(FullModel_VAR$Env_preview)),
          plotOutput(paste(Mod,name,"plotEnvironment", sep="_"))

      )
      
    ) # End span
  ) # End return
  
}