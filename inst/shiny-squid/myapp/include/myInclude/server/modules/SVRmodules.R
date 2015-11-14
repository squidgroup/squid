# Sever function for all modules
SVRmodules <- function(input, session){
  
  return(c(
    
    observe({if(input$Mod1Step2GotoStep1 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 1")}),  
    observe({if(input$Mod1Step1GotoStep2 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 2")}),  
    observe({if(input$Mod1Step3GotoStep2 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 2")}),  
    observe({if(input$Mod1Step2GotoStep3 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 3")}),  
    observe({if(input$Mod1Step4GotoStep3 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 3")}),  
    observe({if(input$Mod1Step3GotoStep4 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 4")})      
    
  ))
}