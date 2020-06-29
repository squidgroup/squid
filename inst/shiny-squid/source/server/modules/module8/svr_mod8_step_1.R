#Server functions for module 8 step 1
c(
    ######### Set variables ######### 
    Mod8Step1updateVind <- function(input, nb.IS){
    	m <- matrix(rep(0,nb.IS*nb.IS),nb.IS)
    	diag(m)[1] <- input$Mod8Step1_Vi
    	diag(m)[2] <- input$Mod8Step1_Vs1
    	m[2,1]     <- input$Mod8Step1_CorIS1
    	return(m)
    },
    # Set hidden variables
     output$Mod8Step1_hidden <- renderUI({
        list(
          numericInput("Mod8Step1_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod8Step1_NI", "",100),
          numericInput("Mod8Step1_NR", "",20),
          
          matrixInput("Mod8Step1_Vind", value = Mod8Step1updateVind(input, nb.IS), class = "numeric"),
          matrixInput("Mod8Step1_B", value = matrix(c(0,input$Mod8Step1_B1,
                                                      input$Mod8Step1_B2, 
                                                      input$Mod8Step1_B12),1), class = "numeric"),
          
          checkboxInput("Mod8Step1_X1_state", "",      value = TRUE),
          checkboxInput("Mod8Step1_X1_sto_state", "",  value = TRUE),
          checkboxInput("Mod8Step1_X1_sto_shared", "", value = FALSE),
          
          checkboxInput("Mod8Step1_X2_state", "",      value = TRUE),
          checkboxInput("Mod8Step1_X2_sto_state", "",  value = TRUE),
          checkboxInput("Mod8Step1_X2_sto_shared", "", value = FALSE),
          
          checkboxInput("Mod8Step1_X_Interaction", "", value = TRUE)
        )
     }),
    outputOptions(output, "Mod8Step1_hidden", suspendWhenHidden = FALSE),
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod8Step1_output <- reactive({
        
        if (input$Mod8Step1_Run == 0) # if Run button is pressed
            return(NULL)
        
        isolate({ 
            
            updateCheckboxInput(session, "isRunning", value = TRUE)
            
            # Call app main function
            data <- squid::squidR(input, module = "Mod8Step1")  
            
            LMR      <- lme4::lmer(Phenotype ~ 1 + X1*X2 + (1 + X1|Individual), data = data$sampled_data)
            FIXEF    <- lme4::fixef(LMR)
            SE.FIXEF <- arm::se.fixef(LMR)
            RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
            
            data$Vi_1      <- round(RANDEF[1],2)
            data$Vs_1      <- round(RANDEF[2],2)
            data$COVis1_1  <- round(RANDEF[3],2)
            data$Vr_1      <- round(RANDEF[4],2) 

            data$B0_1      <- round(FIXEF["(Intercept)"],2)
            data$se.B0_1   <- round(SE.FIXEF["(Intercept)"],2)
                        
            data$B1_1      <- round(FIXEF["X1"],2)
            data$se.B1_1   <- round(SE.FIXEF["X1"],2)
            
            data$B2_1      <- round(FIXEF["X2"],2)
            data$se.B2_1   <- round(SE.FIXEF["X2"],2)
            
            data$B12_1     <- round(FIXEF["X1:X2"],2)
            data$se.B12_1  <- round(SE.FIXEF["X1:X2"],2)
            
            #--------------------
            
            LMR2     <- lme4::lmer(Phenotype ~ 1 + X1*X2 + (1|Individual), data = data$sampled_data)
            FIXEF    <- lme4::fixef(LMR2)
            SE.FIXEF <- arm::se.fixef(LMR2)
            RANDEF   <- as.data.frame(lme4::VarCorr(LMR2))$vcov
            
            data$Vi_2      <- round(RANDEF[1],2)
            data$Vr_2      <- round(RANDEF[2],2)
            
            data$B0_2      <- round(FIXEF["(Intercept)"],2)
            data$se.B0_2   <- round(SE.FIXEF["(Intercept)"],2)
            
            data$B1_2      <- round(FIXEF["X1"],2)
            data$se.B1_2   <- round(SE.FIXEF["X1"],2)
            
            data$B2_2      <- round(FIXEF["X2"],2)
            data$se.B2_2   <- round(SE.FIXEF["X2"],2)
            
            data$B12_2     <- round(FIXEF["X1:X2"],2)
            data$se.B12_2  <- round(SE.FIXEF["X1:X2"],2)
            
            updateCheckboxInput(session, "isRunning", value = FALSE)
            
            return(data)
        })  
    }),
    
    
    output$Mod8Step1_summary_table1 <- renderUI({ 
        
        data <- Mod8Step1_output()
        
        myTable <- data.frame(
            "Parameter" = c("Parameter", 
                            "Fixed effects",
                            paste0("Mean of the trait $",EQ1$mean0,"$"),
                            paste0("Population slope of $",EQ2$env1,"$ ($",EQ1$mean1,"$)"),
                            paste0("Population slope of $",EQ2$env2,"$ ($",EQ1$mean2,"$)"),
                            paste0("Population slope of $",EQ2$env12,"$ ($",EQ1$mean12,"$)")),
            
            "True" = c("True",
                       "",
                       0,
                       input$Mod8Step1_B1,
                       input$Mod8Step1_B2,
                       input$Mod8Step1_B12),
            
            "Estimated"      = c("Estimated : Incomplete",
                                 "",
                                 paste("$",NOT$mean,"'_{0}$=", ifelse(!is.null(data), paste(data$B0_1,"\U00b1", data$se.B0_1),"...")),
                                 paste("$",NOT$mean,"'_{1}$=", ifelse(!is.null(data), paste(data$B1_1,"\U00b1", data$se.B1_1),"...")),
                                 paste("$",NOT$mean,"'_{2}$=", ifelse(!is.null(data), paste(data$B2_1,"\U00b1", data$se.B2_1),"...")),
                                 paste("$",NOT$mean,"'_{12}$=", ifelse(!is.null(data), paste(data$B12_1,"\U00b1", data$se.B12_1),"..."))), 
            
            "Estimated_full" = c("Estimated : Full",
                                 "",
                                 paste("$",NOT$mean,"'_{0}$=", ifelse(!is.null(data), paste(data$B0_2,"\U00b1", data$se.B0_2),"...")),
                                 paste("$",NOT$mean,"'_{1}$=", ifelse(!is.null(data), paste(data$B1_2,"\U00b1", data$se.B1_2),"...")),
                                 paste("$",NOT$mean,"'_{2}$=", ifelse(!is.null(data), paste(data$B2_2,"\U00b1", data$se.B2_2),"...")),
                                 paste("$",NOT$mean,"'_{12}$=", ifelse(!is.null(data), paste(data$B12_2,"\U00b1", data$se.B12_2),"..."))))
        
        getTable(myTable, header = TRUE) 
    })
   
   
) # End return
