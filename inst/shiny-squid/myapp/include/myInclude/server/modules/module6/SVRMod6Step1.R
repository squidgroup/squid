#Server functions for module 6 step 1
SVRMod6Step1 <- function(input, output, session, Modules_VAR, nb.IS, color){
  
  return(c(
  
    ######### Set variables #########    
      # Set hidden variables (Tmax, Vi, ES_state, ES_ran_V and NR)
       output$Mod3Step1_hidden <- renderUI({
          list(
            numericInput("Mod6Step1_Tmax", "", Modules_VAR$Tmax$max),
            numericInput("Mod6Step1_NI", "", 100),
            matrixInput2("Mod6Step1_Vind", "",data.frame(matrix(c(input$Mod3Step1_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS))),
            matrixInput2("Mod3Step1_B", "",data.frame(matrix(c(0,input$Mod3Step1_beta1,0,0),1))),
            checkboxInput("Mod3Step1_X1_state", "", value = TRUE),
            checkboxInput("Mod3Step1_X1_ran_state", "", value = TRUE),
            checkboxInput("Mod3Step1_X1_ran_shared", "", value = TRUE),
            numericInput("Mod3Step1_X1_ran_V","", 1, min = 0, max = 1, step = 0.001),
            checkboxInput("Mod3Step1_Dtime_Ind", "", value = FALSE)
          )
        }),
 	outputOptions(output, "Mod3Step1_hidden", suspendWhenHidden = FALSE)

    

  )) # End return
}