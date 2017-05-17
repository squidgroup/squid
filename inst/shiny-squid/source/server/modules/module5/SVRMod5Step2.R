#Server functions for module 5 step 2
c(
    ######### Set variables #########
    # Set hidden variables
    Mod5Step2updateVind <- function(input, nb.IS){
      df <- matrix(rep(0,nb.IS*nb.IS),nb.IS)
      diag(df)[1] <- input$Mod5Step2_Vi
      return(as.data.frame(df))
    },
    # Set hidden variables
    output$Mod5Step2_hidden <- renderUI({
      list(
        numericInput("Mod5Step2_Tmax", "", Modules_VAR$Tmax$max),
        matrixInput2("Mod5Step2_Vind", "", Mod5Step1updateVind(input, nb.IS)),
        matrixInput2("Mod5Step2_B",    "", data.frame(matrix(c(0,input$Mod5Step2_B1,input$Mod5Step2_B2,input$Mod5Step2_B12),1))),
        checkboxInput("Mod5Step2_X1_state", "", value = TRUE),
        checkboxInput("Mod5Step2_X1_sto_state", "", value = TRUE),
        checkboxInput("Mod5Step2_X2_state", "", value = TRUE),
        checkboxInput("Mod5Step2_X2_sto_state", "", value = TRUE),
        checkboxInput("Mod5Step2_X_Interaction", "", value = TRUE)
      )
    }),
    outputOptions(output, "Mod5Step2_hidden", suspendWhenHidden = FALSE),
    
    Mod5Step2_output <- reactive({
      
      if (input$Mod5Step2_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- squid::squidR(input, module = "Mod5Step2")  
        
        LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + (1|Individual), data = data$sampled_data)
        FIXEF    <- lme4::fixef(LMR)
        SE.FIXEF <- arm::se.fixef(LMR)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi      <- round(RANDEF[1],2)
        data$Vr      <- round(RANDEF[2],2)
        data$VE      <- 0
        
        data$B1      <- round(FIXEF["X1"],2)
        data$se.B1   <- round(SE.FIXEF["X1"],2)
        
        data$B2      <- round(FIXEF["X2"],2)
        data$se.B2   <- round(SE.FIXEF["X2"],2)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    
    Mod5Step2_output2 <- reactive({
      
      if (input$Mod5Step2_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- squid::squidR(input, module = "Mod5Step2")  
        
        LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + X1X2 + (1|Individual), data = data$sampled_data)
        FIXEF    <- lme4::fixef(LMR)
        SE.FIXEF <- arm::se.fixef(LMR)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi      <- round(RANDEF[1],2)
        data$Vr      <- round(RANDEF[2],2) 
        data$VE      <- 0
        
        data$B1      <- round(FIXEF["X1"],2)
        data$se.B1   <- round(SE.FIXEF["X1"],2)
        
        data$B2      <- round(FIXEF["X2"],2)
        data$se.B2   <- round(SE.FIXEF["X2"],2)
        
        data$B12     <- round(FIXEF["X1X2"],2)
        data$se.B12  <- round(SE.FIXEF["X1X2"],2)
        
        data$sampled_data$Phenotype_predict <- stats::predict(LMR)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    
    
    Mod5Step2_table <- reactive({
      
      data    <- Mod5Step2_output()
      data2   <- Mod5Step2_output2()
      
      myTable <- data.frame(
        "True" = c("True Value",
                         paste0("$",EQ3$mean1,"$ = ",input$Mod5Step2_B[2]),
                         paste0("$",EQ3$mean2,"$ = ",input$Mod5Step2_B[3]),
                         paste0("$",EQ3$mean12,"$ = ",input$Mod5Step2_B[4]),
                         paste0("Individual variance ($V_",NOT$devI,"$) = ",input$Mod5Step2_Vi),
                         paste0("Environmental variance ($V_",NOT$envEffect,"$) = ", sum(input$Mod5Step2_B[2:nb.IS]^2)),
                         paste0("Measurement variance ($V_",NOT$mError,"$) = ",input$Mod5Step2_Ve)),
        
        "Estimated1"  = c("Estimated  Values (ignoring interaction)",
                         paste0("$",NOT$mean,"'_1$ = ",ifelse(!is.null(data),paste(data$B1,"\U00b1", data$se.B1),"...")),
                         paste0("$",NOT$mean,"'_2$ = ",ifelse(!is.null(data),paste(data$B2,"\U00b1", data$se.B2),"...")),
                         "-",
                         paste0("Individual variance ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi,"...")),
                         paste0("Environmental variance accounted for = ", ifelse(!is.null(data),sum(c(data$B1,data$B2)^2),"...")),
                         paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data),data$Vr,"..."))),
        
        "Estimated2"  = c("Estimated Values (from full model)",
                         paste0("$",NOT$mean,"'_1$ = ",ifelse(!is.null(data2),paste(data2$B1,"\U00b1", data2$se.B1),"...")),
                         paste0("$",NOT$mean,"'_2$ = ",ifelse(!is.null(data2),paste(data2$B2,"\U00b1", data2$se.B2),"...")),
                         paste0("$",NOT$mean,"'_12$ = ",ifelse(!is.null(data2),paste(data2$B12,"\U00b1", data2$se.B12),"...")),
                         paste0("Individual variance ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data2),data2$Vi,"...")),
                         paste0("Environmental variance accounted for = ", ifelse(!is.null(data2),sum(c(data2$B1, data2$B2, data2$B12)^2),"...")),
                         paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data2),data2$Vr,"..."))))  
      
      return(getTable(myTable, header = TRUE))
    }),
    
    # Display results (table)
    output$Mod5Step2_summary_table <- renderUI({Mod5Step2_table()}),
    
    # Display 3D figure
    output$Mod5Step2_3D_scatterplot  <- renderScatterplotThree({
      
      data <- Mod5Step2_output2()$sampled_data
      
      if (!is.null(data)) {
        
        data <- as.data.table(data)
        dt   <- data.table("Individual" = unique(data$Individual),
                           "Colour"     = rainbow(length(unique(data$Individual))))
        
        setkey(data, Individual); setkey(dt, Individual);
        data <- merge(data, dt, all.x = TRUE)
        
        threejs::scatterplot3js(x = data$X1, y = data$Phenotype_predict, z = data$X2,
                                color = data$Colour,
                                axisLabels = c("X1", "Phenotype", "X2"),
                                renderer   = "canvas")
      }else{
        print(plot(0,type = 'n',ann = FALSE, xaxt = "n", yaxt = "n"))
      }
    }),
    output$Mod5Step2_3D <- renderUI({scatterplotThreeOutput("Mod5Step2_3D_scatterplot")})
   
) # End return
