UImodule3 <- function(){

  
  return(
    wellPanel( 
      
      # Title
      h3(HTML(module3_txt$title)),
      
      tabsetPanel(id = "Module3TabsetPanel", type = "pills", selected = "Step 1",
                  
        # Step 1 ------------------------------------------------------------
        tabPanel("Step 1", UIMod3Step1()),    
        
        # Step 2 ------------------------------------------------------------
        tabPanel("Step 2", UIMod3Step2())#,
        
        # Step 3 ------------------------------------------------------------
        # tabPanel("Step 3", UIMod3Step3())
      
      )
    )
  )
  
}