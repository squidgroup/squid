UIenvironment <- function(Mod, name, isShared){

  return(
    span(
      fluidRow(
         column(6,
          wellPanel( 
             UIspecificEnvironment(Mod, name, "ran", isShared)
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
        condition = paste("input.",Mod,"_",name,"_ran_state == 1 || input.",
                          Mod,"_",name,"_lin_state == 1 || input.",
                          Mod,"_",name,"_cyc_state == 1", sep=""),
          
#           bsButton(paste(Mod,name,"VisualizeButton", sep="_"), label = "Visualize Environment", icon= FullModel_VAR$Run$icon, style = FullModel_VAR$Run$style),
#           p(),
          
          plotOutput(paste(Mod,name,"plotEnvironment", sep="_"))

      )
      
    ) # End span
  ) # End return
  
}


    
    
    