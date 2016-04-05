#Server functions for module 3 step 3
c(
    ######### Set variables #########
    # Set hidden variables
    output$Mod3Step3_hidden <- renderUI({
      list(
        numericInput("Mod3Step3_Tmax", "", Modules_VAR$Tmax$max),
        numericInput("Mod3Step3_NI", "", 100),
        matrixInput2("Mod3Step3_Vind", "",data.frame(matrix(c(input$Mod3Step3_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS))),
        matrixInput2("Mod3Step3_B", "",data.frame(matrix(c(0,sqrt(input$Mod3Step3_Vbx),0,0),1))), 
        
        checkboxInput("Mod3Step3_X1_state", "", value = TRUE),
        
        checkboxInput("Mod3Step3_X1_sto_state", "", value = ifelse(input$Mod3Step3_X_select %in% c("sto","auto"),TRUE,FALSE)),
        checkboxInput("Mod3Step3_X1_sto_autocor_state", "", value = ifelse(input$Mod3Step3_X_select == "auto",TRUE,FALSE)),
        checkboxInput("Mod3Step3_X1_sto_shared", "", value = input$Mod3Step3_X_Shared),
        
        checkboxInput("Mod3Step3_X1_lin_state", "", value = ifelse(input$Mod3Step3_X_select == "lin",TRUE,FALSE)),
        checkboxInput("Mod3Step3_X1_lin_shared", "", value = input$Mod3Step3_X_Shared),
        
        checkboxInput("Mod3Step3_X1_cyc_state", "", value = ifelse(input$Mod3Step3_X_select == "cyc",TRUE,FALSE)),
        checkboxInput("Mod3Step3_X1_cyc_shared", "", value = input$Mod3Step3_X_Shared),
        
        checkboxInput("Mod3Step3_ST_ind", "", value = FALSE)
      )
    }),
    outputOptions(output, "Mod3Step3_hidden", suspendWhenHidden = FALSE),
    
    output$Mod3Step3_X1_plot <- renderPlot({SQUID::squidR(input, module="Mod3Step3" , X_previsualization="X1")}),
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod3Step3_output <- reactive({
      if(input$Mod3Step3_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- SQUID::squidR(input, module="Mod3Step3")  
        
        LMR      <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = data$sampled_Data)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi        <- round(RANDEF[1],2)
        data$Vr        <- round(RANDEF[2],2)
        
        LMR2      <- lme4::lmer(Phenotype ~ 0 + X1 + (1|Individual), data = data$sampled_Data)
        RANDEF2   <- as.data.frame(lme4::VarCorr(LMR2))$vcov
        
        data$Vi_2      <- round(RANDEF2[1],2)
        data$Vr_2      <- round(RANDEF2[2],2)
        data$B1_2      <- round(lme4::fixef(LMR2)[1],2)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })
    }),
    
    Mod3Step3_output_2 <- reactive({
      if(input$Mod3Step3_Run2 == 0) # if Run button is pressed
        return(NULL)
      
      isolate({
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- SQUID::squidR(input, module="Mod3Step3")
        
        LMR      <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = data$sampled_Data)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi        <- round(RANDEF[1],2)
        data$Vr        <- round(RANDEF[2],2)
        
        data$sampled_Data$X1 <- input$Mod3Step3_Vbx_proportion * data$sampled_Data$X1
        
        LMR2      <- lme4::lmer(Phenotype ~ 0 + X1 + (1|Individual), data = data$sampled_Data)
        RANDEF2   <- as.data.frame(lme4::VarCorr(LMR2))$vcov
        
        data$Vi_2      <- round(RANDEF2[1],2)
        data$Vr_2      <- round(RANDEF2[2],2)
        data$B1_2      <- round(lme4::fixef(LMR2)[1],2)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    
    output$Mod3Step3_previewPlot <- renderPlot({
      
      input$Mod3Step3_previewPlot
      
      myInput <- list("Mod3Step3_Preview_Tmax"   = Modules_VAR$Tmax$max,
                      "Mod3Step3_Preview_NI"     = input$Mod3Step3_NI,
                      "Mod3Step3_Preview_Vhsi"   = input$Mod3Step3_Vhsi,
                      "Mod3Step3_Preview_NR"     = input$Mod3Step3_NR,
                      "Mod3Step3_Preview_ST_ind" = FALSE
      )
      # Call app main function
      data <- SQUID::squidR(myInput, module="Mod3Step3_Preview", plot=TRUE)
      print(data$myPlot$plotSampTime)
    }),
    
    Mod3Step3_table <- function(data){
      
      myTable <- data.frame("True"       = c("True",
                                             paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod3Step3_Vi),
                                             paste("Measurement error variance ($V_",NOT$error,"$) =",input$Mod3Step3_Ve),
                                             paste("Phenotypic variance due to known environment ($V_{",EQ3$mean1,EQ2$env1,"}$) =",input$Mod3Step3_Vbx),
                                             paste("Mean environmental effect ($",EQ3$mean1,"$) =",round(input$Mod3Step3_B[2],2))),
                            "Totally unknown environment" = c("Totally unknown environment",
                                                              paste("Individual variance ($V'_",NOT$devI,"$) = "      ,ifelse(!is.null(data),data$Vi,"...")),
                                                              paste("Residual variance ($V'_",NOT$residual,"$) = "        ,ifelse(!is.null(data),data$Vr,"...")),
                                                              "",
                                                              ""),
                            "Environment known" = c("Environment known",
                                                    paste("Individual variance ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi_2,"...")),
                                                    paste("Residual variance ($V'_",NOT$residual,"$) = ", ifelse(!is.null(data),data$Vr_2,"...")),
                                                    paste0("Phenotypic variance due to known environment ($V_{",EQ3$mean1,EQ2$env1,"}$) = ", ifelse(!is.null(data),data$B1_2^2,"...")),
                                                    paste0("Mean environmental effect ($",NOT$mean,"'_1$) =", ifelse(!is.null(data),data$B1_2,"...")))
      )  
    
        return(getTable(myTable, header=TRUE))
    },  
    
    # Display results (table)
    output$Mod3Step3_summary_table <- renderUI({
      data   <- Mod3Step3_output()
      Mod3Step3_table(data)
    }),
    output$Mod3Step3_summary_table_2 <- renderUI({
      data   <- Mod3Step3_output_2()
      Mod3Step3_table(data)
    }),

    observe({
      updateSliderInput(session, "Mod3Step3_Vhsi", value = input$Mod3Step3_Vhsi2)
    }),

    observe({
      updateSliderInput(session, "Mod3Step3_Vhsi2", value = input$Mod3Step3_Vhsi)
    }),

    output$Mod3Step3_Vi_proportion <- renderText({paste0("(",round(input$Mod3Step3_Vi / (input$Mod3Step3_Vi + input$Mod3Step3_Vbx + input$Mod3Step3_Ve),2)*100,"%)")}),
    output$Mod3Step3_Ve_proportion <- renderText({paste0("(",round(input$Mod3Step3_Ve / (input$Mod3Step3_Vi + input$Mod3Step3_Vbx + input$Mod3Step3_Ve),2)*100,"%)")}),
    output$Mod3Step3_Vbx_proportion <- renderText({paste0("(",round(input$Mod3Step3_Vbx / (input$Mod3Step3_Vi + input$Mod3Step3_Vbx + input$Mod3Step3_Ve),2)*100,"%)")}),

    ######### Manage errors #########
    # display error message
    observe({
      if(
        !testInput(input$Mod3Step3_X1_sto_V, FullModel_VAR$stoV, FALSE, FALSE) ||
        !testInput(input$Mod3Step3_X1_sto_corr, FullModel_VAR$stoCorr, FALSE, FALSE)){
        updateButton(session, "Mod3Step3_Run", disabled = TRUE, style = Modules_VAR$Run$invalidStyle)
      }else{
        updateButton(session, "Mod3Step3_Run", disabled = FALSE, style = Modules_VAR$Run$style)
      }
    }),

    output$Mod3Step3_error_sto_V     <- renderUI({testInput(input$Mod3Step3_X1_sto_V, FullModel_VAR$stoV, FALSE, TRUE)}),
    output$Mod3Step3_error_sto_corr  <- renderUI({testInput(input$Mod3Step3_X1_sto_corr, FullModel_VAR$stoCorr, FALSE, TRUE)})
    
  ) # End return