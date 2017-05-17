#Server functions for module 5 step 2
c(
    ######### Set variables #########
    # Set hidden variables
    output$Mod5Step2_hidden <- renderUI({
    	list(
    		numericInput("Mod5Step2_Tmax", "", Modules_VAR$Tmax$max),
    		matrixInput2("Mod5Step2_B",    "", data.frame(matrix(c(0,input$Mod5Step2_B1,input$Mod5Step2_B2,input$Mod5Step2_B12),1)))
    	)
    }),
    outputOptions(output, "Mod5Step2_hidden", suspendWhenHidden = FALSE)
   
) # End return
