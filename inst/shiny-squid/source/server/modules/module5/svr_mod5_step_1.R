#Server functions for module 5 step 1
c(
    ######### Set variables ######### 
    Mod5Step1updateVind <- function(input, nb.IS){
    	m <- matrix(rep(0,nb.IS*nb.IS),nb.IS)
    	diag(m)[1] <- input$Mod5Step1_Vi
    	return(m)
    },
    # Set hidden variables
     output$Mod5Step1_hidden <- renderUI({
        list(
          numericInput("Mod5Step1_Tmax", "", Modules_VAR$Tmax$max),
          shinyMatrix::matrixInput("Mod5Step1_Vind", value = Mod5Step1updateVind(input, nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod5Step1_B", value = matrix(c(0,input$Mod5Step1_B1,input$Mod5Step1_B2,0),1), class = "numeric"),
          checkboxInput("Mod5Step1_X1_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X1_sto_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X2_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X2_sto_state", "", value = TRUE)
        )
     }),
    outputOptions(output, "Mod5Step1_hidden", suspendWhenHidden = FALSE),
    
   ######### Run simulation #########
   # Run simulation and return results
   Mod5Step1_output <- reactive({
   	
   	if (input$Mod5Step1_Run == 0) # if Run button is pressed
   		   return(NULL)
   	
   	isolate({ 
   		
   		updateCheckboxInput(session, "isRunning", value = TRUE)
   		
   		# Call app main function
   		data <- squid::squidR(input, module = "Mod5Step1")  
   		
   		LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual), data = data$sampled_data)
   		FIXEF    <- lme4::fixef(LMR)
   		SE.FIXEF <- arm::se.fixef(LMR)
   		RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   		
   		data$Vi_1      <- round(RANDEF[1],2)
   		data$Vr_1      <- round(RANDEF[2],2) 
   		
   		data$B1_1      <- round(FIXEF["X1"],2)
   		data$se.B1_1   <- round(SE.FIXEF["X1"],2)
   		
   		
   		LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + (1|Individual), data = data$sampled_data)
   		FIXEF    <- lme4::fixef(LMR)
   		SE.FIXEF <- arm::se.fixef(LMR)
   		RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   		
   		data$Vi_2      <- round(RANDEF[1],2)
   		data$Vr_2      <- round(RANDEF[2],2)
   		
   		data$B1_2      <- round(FIXEF["X1"],2)
   		data$se.B1_2   <- round(SE.FIXEF["X1"],2)
   		
   		data$B2_2      <- round(FIXEF["X2"],2)
   		data$se.B2_2   <- round(SE.FIXEF["X2"],2)

   		updateCheckboxInput(session, "isRunning", value = FALSE)
   		
   		return(data)
   	})  
   }),
   

   output$Mod5Step1_summary_table1 <- renderUI({ 
      
      data <- Mod5Step1_output()
      
      myTable <- data.frame(
         "True"       = c(paste0("$",EQ3$mean1,"$ = ",input$Mod5Step1_B[2]),
                          paste0("Individual variance ($V_",NOT$devI,"$) = ",input$Mod5Step1_Vi),
                          paste0("Measurement variance ($V_",NOT$mError,"$) = ",input$Mod5Step1_Ve)),
         "Estimated"  = c(paste0("$",NOT$mean,"'_1$ = ",ifelse(!is.null(data),paste(data$B1_1,"\U00b1", data$se.B1_1),"...")),
                          paste0("Individual variance ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi_1,"...")),
                          paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data),data$Vr_1,"..."))))
       
      getTable(myTable) 
   }),
   
   
   
   output$Mod5Step1_summary_table2 <- renderUI({ 
      
      data <- Mod5Step1_output()
      
      myTable <- data.frame(
         "True"       = c(paste0("$",EQ3$mean1,"$ = ",input$Mod5Step1_B[2]),
                          paste0("$",EQ3$mean2,"$ = ",input$Mod5Step1_B[3]),
                          paste0("Individual variance ($V_",NOT$devI,"$) = ",input$Mod5Step1_Vi),
                          paste0("Measurement variance ($V_",NOT$mError,"$) = ",input$Mod5Step1_Ve)),
         "Estimated"  = c(paste0("$",NOT$mean,"'_1$ = ",ifelse(!is.null(data),paste(data$B1_2,"\U00b1", data$se.B1_2),"...")),
                          paste0("$",NOT$mean,"'_2$ = ",ifelse(!is.null(data),paste(data$B2_2,"\U00b1", data$se.B2_2),"...")),
                          paste0("Individual variance ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi_2,"...")),
                          paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data),data$Vr_2,"..."))))
      
      getTable(myTable) 
   }),

   # Display 3D figure
   output$Mod5Step1_3D_1 <- renderPlotly({
      
      data <- Mod5Step1_output()$sampled_data
      
      isolate({
         
         if (!is.null(data)) {
            
            X_seq <- seq(from = min(data[ , c("X1", "X2")]), to = max(data[ , c("X1", "X2")]), length.out = 10)
            
            predictors      <- cbind("intecept" = 1, expand.grid("X1" = X_seq, "X2" = X_seq))
            predictors$X1X2 <- predictors$X1 * predictors$X2
            
            Phenotype_mean <- as.matrix(predictors) %*% as.vector(input$Mod5Step1_B)
            Phenotype_mean <- t(matrix(Phenotype_mean, nrow = length(X_seq), ncol = length(X_seq)))
            
            plotly::plot_ly(hoverinfo = "none")  %>%
               plotly::add_surface(x = X_seq, y = X_seq, z = Phenotype_mean, opacity = 0.7,
                           colorscale = list(c(0, 1), c("black", "black"))) %>%
               plotly::add_markers(data = data, x = ~X1, y = ~X2, z = ~Phenotype, color = ~Individual, size=4) %>%
               plotly::layout(showlegend = FALSE) %>%
               plotly::hide_colorbar()
            
         }else{defaultPlot()}
      
      })
   }),
   
   output$Mod5Step1_3D_2 <- renderPlotly({
      
      data <- Mod5Step1_output()$sampled_data
      
      isolate({
      
         if (!is.null(data)) {
            
            X_seq <- seq(from = min(data[ , c("X1", "X2")]), to = max(data[ , c("X1", "X2")]), length.out = 10)
            
            predictors      <- cbind("intecept" = 1, expand.grid("X1" = X_seq, "X2" = X_seq))
            predictors$X1X2 <- predictors$X1 * predictors$X2
            
            Phenotype_mean <- as.matrix(predictors) %*% as.vector(input$Mod5Step1_B)
            Phenotype_mean <- t(matrix(Phenotype_mean, nrow = length(X_seq), ncol = length(X_seq)))
            
            All.I <- sort(unique(data$I))
            I.min <- min(All.I)
            I.max <- max(All.I)
            I.med <- All.I[round(length(All.I) / 2)]
            
            data <- as.data.table(data)
            data <- data[I %in% c(I.min, I.med, I.max)]
            
            plotly::plot_ly(hoverinfo = "none")  %>%
               plotly::add_surface(x = X_seq, y = X_seq, z = Phenotype_mean, opacity = 0.7,
                           colorscale = list(c(0, 1), c("black", "black"))) %>%
               plotly::add_markers(data = data, x = ~X1, y = ~X2, z = ~Phenotype, color = ~Individual, size=4) %>%
               plotly::layout(showlegend = FALSE) %>%
               plotly::hide_colorbar()
            
         }else{defaultPlot()}
         
      })
   })

) # End return
