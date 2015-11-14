
UImodules <- function(){
  
  return(
    
    fixedPage( HTML("<div>"),                                 
       
       navlistPanel( 
         
         id       = "modulesNavList",
         selected = "Module 1",
         well     = TRUE,  
         fluid    = FALSE,
         widths   = c(2,9),
                
         "Modules",
         
         # Module 1
         tabPanel("Module 1",UImodule1()), # END Module 1               
         
         # Module 2
         tabPanel("Module 2",UImodule2()), # END Module 2
         
         # Module 3
         tabPanel("Module 3",UImodule3()), # END Module 3
         
         # Module 4
         tabPanel("Module 4",UImodule4()), # END Module 4
         
         # Module 5
         tabPanel("Module 5",UImodule5()), # END Module 5
         
         # Module 6
         tabPanel("Module 6",UImodule6()), # END Module 6
         
         # Module 7
         tabPanel("Module 7",UImodule7()), # END Module 7
         
         # Module 8
         tabPanel("Module 8",UImodule8()), # END Module 8
         
         # Module 9
         tabPanel("Module 9",UImodule9()) # END Module 9
         
#          # Module 10
#          tabPanel("Module 10",module10()) # END Module 10
         
         
      ),
      HTML("</div>") # END div -> id=portal
    )
  )

}
             