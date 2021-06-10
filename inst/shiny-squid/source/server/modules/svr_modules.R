# Sever function for all modules
c(
  #### Module 1 ####
  observe({if (input$Mod1Step2GotoStep1 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod1Step1GotoStep2 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod1Step3GotoStep2 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod1Step2GotoStep3 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 3")}),
  observe({if (input$Mod1Step4GotoStep3 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 3")}),
  observe({if (input$Mod1Step3GotoStep4 != 0) updateTabsetPanel(session, "Module1TabsetPanel", selected = "Step 4")}),
  
  #### Module 2 ####
  observe({if (input$Mod2Step2GotoStep1 != 0) updateTabsetPanel(session, "Module2TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod2Step1GotoStep2 != 0) updateTabsetPanel(session, "Module2TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod2Step3GotoStep2 != 0) updateTabsetPanel(session, "Module2TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod2Step2GotoStep3 != 0) updateTabsetPanel(session, "Module2TabsetPanel", selected = "Step 3")}),
  
  #### Module 3 ####
  observe({if (input$Mod3Step2GotoStep1 != 0) updateTabsetPanel(session, "Module3TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod3Step1GotoStep2 != 0) updateTabsetPanel(session, "Module3TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod3Step3GotoStep2 != 0) updateTabsetPanel(session, "Module3TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod3Step2GotoStep3 != 0) updateTabsetPanel(session, "Module3TabsetPanel", selected = "Step 3")}),
  
  #### Module 4 ####
  observe({if (input$Mod4Step2GotoStep1 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod4Step1GotoStep2 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod4Step3GotoStep2 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod4Step2GotoStep3 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 3")}),
  observe({if (input$Mod4Step4GotoStep3 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 3")}),
  observe({if (input$Mod4Step3GotoStep4 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 4")}),
  observe({if (input$Mod4Step5GotoStep4 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 4")}),
  observe({if (input$Mod4Step4GotoStep5 != 0) updateTabsetPanel(session, "Module4TabsetPanel", selected = "Step 5")}),

  #### Module 5 ####
  observe({if (input$Mod5Step2GotoStep1 != 0) updateTabsetPanel(session, "Module5TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod5Step1GotoStep2 != 0) updateTabsetPanel(session, "Module5TabsetPanel", selected = "Step 2")}),
  
  #### Module 6 ####
  observe({if (input$Mod6Step2GotoStep1 != 0) updateTabsetPanel(session, "Module6TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod6Step1GotoStep2 != 0) updateTabsetPanel(session, "Module6TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod6Step3GotoStep2 != 0) updateTabsetPanel(session, "Module6TabsetPanel", selected = "Step 2")}),
  observe({if (input$Mod6Step2GotoStep3 != 0) updateTabsetPanel(session, "Module6TabsetPanel", selected = "Step 3")}),

  # #### Module 8 ####
  observe({if (input$Mod8Step2GotoStep1 != 0) updateTabsetPanel(session, "Module8TabsetPanel", selected = "Step 1")}),
  observe({if (input$Mod8Step1GotoStep2 != 0) updateTabsetPanel(session, "Module8TabsetPanel", selected = "Step 2")})
  
)