UImodule1 <- function(){
  
  return(
    wellPanel( 
          
      # Title
      h3(HTML(module1_txt$title)),
      p(HTML(module1_txt$goal)), 
      
      tabsetPanel(id = "Module1TabsetPanel", type = "pills", selected = "Step 1",
      
        # Step 1 ------------------------------------------------------------
        tabPanel("Step 1", UIMod1Step1()),    
                  
        # Step 2 ------------------------------------------------------------
        tabPanel("Step 2", UIMod1Step2()),
        
        # Step 3 ------------------------------------------------------------
        tabPanel("Step 3",UIMod1Step3()),
        
        # Step 4 ------------------------------------------------------------
        tabPanel("Step 4",UIMod1Step4())

      ) # End tabsetPanel
    ) # End Wellpanel
  ) # End return
  
}