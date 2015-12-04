UImodule6 <- function(){
  
  
  # source text
  
  return(
    wellPanel( 
      
      # Title
      h3(HTML(module6_txt$title)),
      p(HTML(module6_txt$goal)), 
      
      tabsetPanel(id = "Module6TabsetPanel", type = "pills", selected = "Step 1",
                  
                  # Step 1 ------------------------------------------------------------
                  tabPanel("Step 1", UIMod6Step1())    
                  
#                   # Step 2 ------------------------------------------------------------
#                   tabPanel("Step 2", UIMod1Step2()),
#                   
#                   # Step 3 ------------------------------------------------------------
#                   tabPanel("Step 3",UIMod1Step3()),
#                   
#                   # Step 4 ------------------------------------------------------------
#                   tabPanel("Step 4",UIMod1Step4())
                  
      ) # End tabsetPanel
    ) # End Wellpanel    
  )
  
}