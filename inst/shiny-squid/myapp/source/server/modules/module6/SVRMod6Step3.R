#Server functions for module 6 step 3
c(

    ######### Set variables #########  
    Mod6Step3updateVind <- function(nb.IS){
      df <- matrix(rep(0,nb.IS^2),nb.IS)
      diag(df)[1] <- 0.5
      diag(df)[2] <- 0.5
      df[2,1]     <- 0.5
      return(as.data.frame(df))
    },
      # Set hidden variables
       output$Mod6Step3_hidden <- renderUI({
          list(
            numericInput("Mod6Step3_NP", "", 100),
            numericInput("Mod6Step3_Tmax", "", Modules_VAR$Tmax$max),
            numericInput("Mod6Step3_Vme", "", 0.05),
            matrixInput2("Mod6Step3_Vind", "", Mod6Step3updateVind(nb.IS)),
            matrixInput2("Mod6Step3_B", "",data.frame(matrix(c(0.5,0.5,0,0),1))),
            checkboxInput("Mod6Step3_X1_state", "", value = TRUE),
            checkboxInput("Mod6Step3_X1_ran_state", "", value = TRUE),
            checkboxInput("Mod6Step3_X1_ran_shared", "", value = TRUE),
            numericInput("Mod6Step3_X1_ran_V","", 1, min = 0, max = 1, step = 0.001),
            checkboxInput("Mod6Step3_Dtime_Ind", "", value = FALSE)
          )
      }),
 	    outputOptions(output, "Mod6Step3_hidden", suspendWhenHidden = FALSE),

    # Run simulation and return results
    Mod6Step3_output <- reactive({
      
      if(input$Mod6Step3_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        if(input$Mod6Step3_selector == 100){
          NI <- c(5,10,20)
          NR <- c(20,10,5)
        }
        
        if(input$Mod6Step3_selector == 225){
          NI <- c(5,15,45)
          NR <- c(45,15,5)
        }
        
        if(input$Mod6Step3_selector == 400){
          NI <- c(10,20,40)
          NR <- c(40,20,10)
        }
        
        if(input$Mod6Step3_selector == 900){
          NI <- c(18,30,50)
          NR <- c(50,30,18)
        }
        
        # Call app main function
        data <- runPowerAnalysis(input, "Mod6Step3", NI, NR)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    

      output$Mod6Step3_summary_variance_table <- renderUI({
        
        myTable <- data.frame(
          "Summary of Variances"= c("$\\text{Fixed effects}$",
                          paste0("Mean of the trait ($",EQ3$mean0,"$)"),
                          paste0("Population-specific slope of the environmental effect ($",EQ3$mean1,"$)"),
                          "$\\text{Random effects}$",
                          paste0("Individual variance ($V_",NOT$devI,"$)"),
                          paste0("Individual-specific response to an environmental effect (random slopes) variance ($V_",NOT$devS,"$)"),
                          paste0("Correlation between individual specific intercepts and slopes ($Cor_{",NOT$devI,",",NOT$devS,"}$)"),
                          paste0("Residual variance ($V_",NOT$error,"$)")),
          "Value" = c("",
                      "0.5",
                      "0.5", 
                      "",
                      "0.5",
                      "0.5",
                      "0.5",
                      "0.05")
        )  
        
        return(getTable(myTable))
        
    }),

    output$Mod6Step3_summary_table <- renderUI({
      
      if(input$Mod6Step3_selector == 100){
        myTable <- data.frame(
          "Observations"           = c(rep(100, 3)),
          "Individuals"            = c(5,10,20),
          "Repeats per individual" = c(20,10,5)
        )  
      }
      
      if(input$Mod6Step3_selector == 225){
        myTable <- data.frame(
          "Observations"           = c(rep(225, 3)),
          "Individuals"            = c(5,15,45),
          "Repeats per individual" = c(45,15,5)
        )  
      }

      if(input$Mod6Step3_selector == 400){
        myTable <- data.frame(
          "Observations"           = c(rep(400, 3)),
          "Individuals"            = c(10,20,40),
          "Repeats per individual" = c(40,20,10)
        )  
      }

      if(input$Mod6Step3_selector == 900){
        myTable <- data.frame(
          "Observations"           = c(rep(900, 3)),
          "Individuals"            = c(18,30,50),
          "Repeats per individual" = c(50,30,18)
        )  
      }
      
      return(getTable(myTable))
      
    }),

    output$Mod6Step3_plot <- renderPlot({ 

      data  <- Mod6Step3_output()
      
      if(!is.null(data)){

        vline.data <- data.frame(z           = rep(c(0.5, 0.5, 0.5),each=3), 
                                 Parameter   = rep(c("CORis", "Vi", "Vs"),each=3))
        
        print(ggplot2::ggplot(data, ggplot2::aes(x=Value)) +
                              ggplot2::geom_histogram(binwidth = 0.5) + 
                              ggplot2::geom_vline(ggplot2::aes(xintercept = z), vline.data, color="red") +
                              ggplot2::facet_grid(Parameter ~ nIndividual + nRecord))
        
      }else{
        print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
      }
    })
) # End return