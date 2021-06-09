#Server functions for module 6 step 3
selectorList <- list("100" = list("NI"=c(5,10,20) , "NR"=c(20,10,5)),
                     "225" = list("NI"=c(5,15,45) , "NR"=c(45,15,5)),
                     "400" = list("NI"=c(10,20,40), "NR"=c(40,20,10)),
                     "900" = list("NI"=c(18,30,50), "NR"=c(50,30,18)))

c(

  ######### Set variables #########  
  Mod6Step3updateVind <- function(nb.IS){
    m <- matrix(rep(0,nb.IS^2),nb.IS)
    diag(m)[1] <- 0.5
    diag(m)[2] <- 0.5
    m[2,1]     <- 0.5
    return(m)
  },
    # Set hidden variables
     output$Mod6Step3_hidden <- renderUI({
        list(
          numericInput("Mod6Step3_NP", "", 100),
          numericInput("Mod6Step3_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod6Step3_Ve", "", 0.05),
          shinyMatrix::matrixInput("Mod6Step3_Vind", value = Mod6Step3updateVind(nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod6Step3_B", value = matrix(c(0.5,0.5,0,0), 1), class = "numeric"),
          checkboxInput("Mod6Step3_X1_state", "", value = TRUE),
          checkboxInput("Mod6Step3_X1_sto_state", "", value = TRUE),
          checkboxInput("Mod6Step3_X1_sto_shared", "", value = TRUE),
          numericInput("Mod6Step3_X1_sto_V","", 1, min = 0, max = 1, step = 0.001),
          checkboxInput("Mod6Step3_ST_ind", "", value = FALSE)
        )
    }),
     outputOptions(output, "Mod6Step3_hidden", suspendWhenHidden = FALSE),
  
  # Run simulation and return results
  Mod6Step3_output <- reactive({
    
    if(input$Mod6Step3_Run == 0) # if Run button is pressed
      return(NULL)
    
    isolate({ 
      
      updateCheckboxInput(session, "isRunning", value = TRUE)
      
       # Call app main function
      data <- runPowerAnalysis(input, "Mod6Step3", 
                               selectorList[[as.character(input$Mod6Step3_selector)]][["NI"]], 
                               selectorList[[as.character(input$Mod6Step3_selector)]][["NR"]])
      
      data$Parameter <- factor(data$Parameter,
                               levels = c("Vi", "Vs", "CORis"))
      
      data$nIndividual <- factor(data$nIndividual,
                                 levels = paste0("NI=",
                                                 selectorList[[as.character(input$Mod6Step3_selector)]][["NI"]]))
      data$nRecord     <- factor(data$nRecord,
                                 levels = paste0("NR=",
                                                 selectorList[[as.character(input$Mod6Step3_selector)]][["NR"]]))
  
      updateCheckboxInput(session, "isRunning", value = FALSE)
      
      return(data)
    })  
  }),
  
  output$Mod6Step3_summary_variance_table <- renderUI({
      
      myTable <- data.frame(
        "Summary of Variances"= c("Summary of Variances",
                                  "$\\text{Fixed effects}$",
                                  paste0("Mean of the trait ($",EQ3$mean0,"$)"),
                                  paste0("Population-specific slope of the environmental effect ($",NOT$mean,"$)"),
                                  "$\\text{Random effects}$",
                                  paste0("Individual variance ($V_",NOT$devI,"$)"),
                                  paste0("Individual-specific response to an environmental effect (random slopes) variance ($V_{",NOT$devS,"}$)"),
                                  paste0("Correlation between individual specific intercepts and slopes ($Cor_{",NOT$devI,",",NOT$devS,"}$)"),
                                  paste0("Measurement error ($V_",NOT$mError,"$)")),
        "Value" = c("Value",
                    "",
                    "0.5",
                    "0.5", 
                    "",
                    "0.5",
                    "0.5",
                    "0.5",
                    "0.05")
      )  
      
      return(getTable(myTable, header=TRUE))
      
  }),
  
  output$Mod6Step3_summary_table <- renderUI({
    
    myTable <- data.frame(
      "Observations"           = rep(as.character(input$Mod6Step3_selector), 3),
      "Individuals"            = selectorList[[as.character(input$Mod6Step3_selector)]][["NI"]],
      "Repeats per individual" = selectorList[[as.character(input$Mod6Step3_selector)]][["NR"]]
    ) 
  
    return(getTable(myTable))
    
  }),

  output$Mod6Step3_plot <- renderPlot({ 
  
    data  <- Mod6Step3_output()
    
    if(!is.null(data)){
  
      vline.data <- data.frame(z           = rep(c(0.5, 0.5, 0.5),each=3), 
                               Parameter   = rep(c("CORis", "Vi", "Vs"),each=3))
    
      ggplot2::ggplot(data, ggplot2::aes(x=Value)) +
              ggplot2::geom_histogram(binwidth = 0.1) + 
              ggplot2::geom_vline(ggplot2::aes(xintercept = z), vline.data, color="red") +
              ggplot2::facet_grid(Parameter ~ nIndividual + nRecord)
      
    }else{
      defaultPlot()
    }
  }),
  
  output$Mod6Step2_Vi_proportion  <- renderText({paste0("(",round(input$Mod6Step2_Vi / (input$Mod6Step2_Vi + input$Mod6Step2_Vbx + input$Mod6Step2_Vs + input$Mod6Step2_Ve),2)*100,"%)")}),
  output$Mod6Step2_Ve_proportion  <- renderText({paste0("(",round(input$Mod6Step2_Ve / (input$Mod6Step2_Vi + input$Mod6Step2_Vbx + input$Mod6Step2_Vs+ input$Mod6Step2_Ve),2)*100,"%)")}),
  output$Mod6Step2_Vbx_proportion <- renderText({paste0("(",round(input$Mod6Step2_Vbx / (input$Mod6Step2_Vi + input$Mod6Step2_Vbx + input$Mod6Step2_Vs + input$Mod6Step2_Ve),2)*100,"%)")}),
  output$Mod6Step2_Vs_proportion  <- renderText({paste0("(",round(input$Mod6Step2_Vs / (input$Mod6Step2_Vi + input$Mod6Step2_Vbx + input$Mod6Step2_Vs + input$Mod6Step2_Ve),2)*100,"%)")})
  
) # End return