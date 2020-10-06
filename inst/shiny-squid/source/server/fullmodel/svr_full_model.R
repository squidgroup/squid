#Server functions for the full model
SVRFullModel <- function(myModule, input, output, session){
  
	myLabels                    <- list()
	
	myLabels$myEquations        <- paste(myModule, "myEquations", sep="_")
  
	myLabels$Tmax               <- paste(myModule, "Tmax", sep="_")
	myLabels$NT                 <- paste(myModule, "NT", sep="_")
	myLabels$NP                 <- paste(myModule, "NP", sep="_")
	myLabels$NI                 <- paste(myModule, "NI", sep="_")
	myLabels$Ve                 <- paste(myModule, "Ve", sep="_")
	myLabels$Ve_input           <- paste(myModule, "Ve_input", sep="_")
	myLabels$Ve_hidden          <- paste(myModule, "Ve_hidden", sep="_")
	myLabels$NG                 <- paste(myModule, "NG", sep="_")
	myLabels$VG                 <- paste(myModule, "VG", sep="_")
	myLabels$VG_input           <- paste(myModule, "VG_input", sep="_")
	myLabels$VG_hidden          <- paste(myModule, "VG_hidden", sep="_")
	myLabels$NR                 <- paste(myModule, "NR", sep="_")
	myLabels$Vhsi               <- paste(myModule, "Vhsi", sep="_")

	myLabels$B                  <- paste(myModule, "B", sep="_")
	myLabels$B_temp             <- paste(myModule, "B_temp", sep="_")
	myLabels$B_UI               <- paste(myModule, "B_UI", sep="_")
	myLabels$error_B            <- paste(myModule, "error_B", sep="_") 
	myLabels$B_UI_hidden        <- paste(myModule, "B_UI_hidden", sep="_")
  
	myLabels$Vind               <- paste(myModule, "Vind", sep="_")
	myLabels$Vind_temp          <- paste(myModule, "Vind_temp", sep="_")
	myLabels$Vind_UI            <- paste(myModule, "Vind_UI", sep="_")
	myLabels$error_Vind         <- paste(myModule, "error_Vind", sep="_") 
	myLabels$Vind_UI_hidden     <- paste(myModule, "Vind_UI_hidden", sep="_")
  
	myLabels$X1_state           <- paste(myModule, "X1_state", sep="_")
	myLabels$X1_sto_state       <- paste(myModule, "X1_sto_state", sep="_")
	myLabels$X1_sto_shared      <- paste(myModule, "X1_sto_shared", sep="_")
	myLabels$X1_sto_V           <- paste(myModule, "X1_sto_V", sep="_")
	myLabels$X1_sto_autocor_state <- paste(myModule, "X1_sto_autocor_state", sep="_")
	myLabels$X1_sto_corr        <- paste(myModule, "X1_sto_corr", sep="_")
	myLabels$X1_lin_state       <- paste(myModule, "X1_lin_state", sep="_")
	myLabels$X1_lin_shared      <- paste(myModule, "X1_lin_shared", sep="_")
	myLabels$X1_lin_intercept   <- paste(myModule, "X1_lin_intercept", sep="_")
	myLabels$X1_lin_slope       <- paste(myModule, "X1_lin_slope", sep="_")
	myLabels$X1_lin_V           <- paste(myModule, "X1_lin_V", sep="_")
	myLabels$X1_cyc_state       <- paste(myModule, "X1_cyc_state", sep="_")
	myLabels$X1_cyc_shared      <- paste(myModule, "X1_cyc_shared", sep="_")
	myLabels$X1_cyc_amplitude   <- paste(myModule, "X1_cyc_amplitude", sep="_")
	myLabels$X1_cyc_period      <- paste(myModule, "X1_cyc_period", sep="_")
	myLabels$X1_cyc_Hshift      <- paste(myModule, "X1_cyc_Hshift", sep="_")
	myLabels$X1_cyc_Vshift      <- paste(myModule, "X1_cyc_Vshift", sep="_")
	myLabels$X1_cyc_V           <- paste(myModule, "X1_cyc_V", sep="_")
	myLabels$X1_plotEnvironment <- paste(myModule, "X1_plotEnvironment", sep="_")
  
	myLabels$X2_state           <- paste(myModule, "X2_state", sep="_")
	myLabels$X2_sto_state       <- paste(myModule, "X2_sto_state", sep="_")
	myLabels$X2_sto_shared      <- paste(myModule, "X2_sto_shared", sep="_")
	myLabels$X2_sto_V           <- paste(myModule, "X2_sto_V", sep="_")
	myLabels$X2_sto_autocor_state <- paste(myModule, "X2_sto_autocor_state", sep="_")
	myLabels$X2_sto_corr        <- paste(myModule, "X2_sto_corr", sep="_")
	myLabels$X2_lin_state       <- paste(myModule, "X2_lin_state", sep="_")
	myLabels$X2_lin_intercept   <- paste(myModule, "X2_lin_intercept", sep="_")
	myLabels$X2_lin_slope       <- paste(myModule, "X2_lin_slope", sep="_")
	myLabels$X2_lin_shared      <- paste(myModule, "X2_lin_shared", sep="_")
	myLabels$X2_lin_V           <- paste(myModule, "X2_lin_V", sep="_")
	myLabels$X2_cyc_state       <- paste(myModule, "X2_cyc_state", sep="_")
	myLabels$X2_cyc_shared      <- paste(myModule, "X2_cyc_shared", sep="_")
	myLabels$X2_cyc_amplitude   <- paste(myModule, "X2_cyc_amplitude", sep="_")
	myLabels$X2_cyc_period      <- paste(myModule, "X2_cyc_period", sep="_")
	myLabels$X2_cyc_Hshift      <- paste(myModule, "X2_cyc_Hshift", sep="_")
	myLabels$X2_cyc_Vshift      <- paste(myModule, "X2_cyc_Vshift", sep="_")
	myLabels$X2_cyc_V           <- paste(myModule, "X2_cyc_V", sep="_")
	myLabels$X2_plotEnvironment <- paste(myModule, "X2_plotEnvironment", sep="_")
  
	myLabels$X_Interaction      <- paste(myModule, "X_Interaction", sep="_")
  
	myLabels$runButton          <- paste(myModule, "runButton", sep="_")
	myLabels$runButtonError     <- paste(myModule, "runButtonError", sep="_")
	myLabels$rerunButton        <- paste(myModule, "rerunButton", sep="_")
	myLabels$rerunButtonError   <- paste(myModule, "rerunButtonError", sep="_")
  
	myLabels$plotEnvironment    <- paste(myModule, "plotEnvironment", sep="_")
	myLabels$plotPhenotype      <- paste(myModule, "plotPhenotype", sep="_")
	myLabels$plotSamples        <- paste(myModule, "plotSamples", sep="_")
  
	myLabels$variancesTable     <- paste(myModule, "variancesTable", sep="_")
  
	myLabels$PB                 <- paste(myModule, "PB", sep="_")
  
	myLabels$modTabsetPanel     <- paste(myModule, "TabsetPanel", sep="_")
  
	myLabels$SampTime           <- paste(myModule, "SampTime", sep="_")
  
	myLabels$NR_ind          <- paste(myModule, "NR_ind", sep="_")
	myLabels$NR_trait        <- paste(myModule, "NR_trait", sep="_")
	myLabels$ST_ind          <- paste(myModule, "ST_ind", sep="_")
	myLabels$ST_trait        <- paste(myModule, "ST_trait", sep="_")
	myLabels$preview_sampling_design     <- paste(myModule, "preview_sampling_design", sep="_")
	myLabels$preview_sampling_design_btn <- paste(myModule, "preview_sampling_design_btn", sep="_")
  
	myLabels$download_sampled      <- paste(myModule, "download_sampled", sep="_")
	myLabels$download_raw          <- paste(myModule, "download_raw", sep="_")
	myLabels$download_Rcode        <- paste(myModule, "download_Rcode", sep="_")
	myLabels$save_inputs           <- paste(myModule, "save_inputs", sep="_")
	myLabels$load_inputs           <- paste(myModule, "load_inputs", sep="_")
  
	myLabels$error_Tmax            <- paste(myModule, "error_Tmax", sep="_")
	myLabels$error_NP              <- paste(myModule, "error_NP", sep="_")    
	myLabels$error_X1_sto_V        <- paste(myModule, "error_X1_sto_V", sep="_")
	myLabels$error_X2_sto_V        <- paste(myModule, "error_X2_sto_V", sep="_")
	myLabels$error_X1_sto_corr     <- paste(myModule, "error_X1_sto_corr", sep="_")
	myLabels$error_X2_sto_corr     <- paste(myModule, "error_X2_sto_corr", sep="_")
	myLabels$error_NI              <- paste(myModule, "error_NI", sep="_")
	myLabels$error_NG              <- paste(myModule, "error_NG", sep="_")
	myLabels$error_Ve              <- paste(myModule, "error_Ve", sep="_")
	myLabels$error_VG              <- paste(myModule, "error_VG", sep="_")
	myLabels$error_NR              <- paste(myModule, "error_NR", sep="_")
	myLabels$error_B               <- paste(myModule, "error_B", sep="_")
	myLabels$error_Vind            <- paste(myModule, "error_Vind", sep="_")
  
	myLabels$Data_Description_Table <- paste(myModule, "Data_Description_Table", sep="_")
  
	myLabels$loader                <- paste(myModule, "loader", sep="_")
  
  return(c(
    
     # Equation  trait 1
     output[[myLabels$myEquations]] <- renderUI({         
        SVRDispayModelEquation(myModule, input)    
      }),
          
     # B vector
     output[[myLabels$B_UI]]<- renderUI({ 
        
        if(myLabels$B_temp %in% names(input)){
          BisNew <- FALSE
          myB    <- input[[myLabels$B_temp]]
        }else{
          BisNew <- TRUE
          myB    <- NULL
        }      
        
        list(SVRGetBMatrix(myLabels$B_temp,
                           input[[myLabels$NT]],
                           input[[myLabels$X1_state]],
                           input[[myLabels$X2_state]],
                           input[[myLabels$X_Interaction]],
                           BisNew,
                           myB), 
             uiOutput(myLabels$error_B))
     }),
      
     output[[myLabels$B_UI_hidden]] <- renderUI({
        
        myNT <- as.numeric(input[[myLabels$NT]])    
        
        if(myLabels$B_temp %in% names(input)){ 
          myB            <- matrix(rep(0,myNT*nb.IS),1)      
          newSize        <- ifelse(myNT*nb.IS > length(input[[myLabels$B_temp]]),length(input[[myLabels$B_temp]]),myNT*nb.IS)      
          myB[1:newSize] <- input[[myLabels$B_temp]][1:newSize]   
        }else{ 
          myB            <- matrix(rep(0,myNT*nb.IS),1)            
          myB[1:nb.IS]   <- FullModel_VAR$B$value[1:nb.IS] 
        }
        
        myB[which(is.na(myB))] <- 0
        shinyMatrix::matrixInput(myLabels$B, value = myB, class = "numeric")
      }),
     outputOptions(output, myLabels$B_UI_hidden, suspendWhenHidden = FALSE),
     
     # Vind matrix
     output[[myLabels$Vind_UI]] <- renderUI({
       
       if(myLabels$Vind_temp %in% names(input)){
         VindisNew <- FALSE
         myVind    <- input[[myLabels$Vind_temp]]
       }else{
         VindisNew <- TRUE
         myVind    <- NULL
       }    
       
       list(SVRGetVindMatrix(myLabels$Vind_temp,
                             input[[myLabels$NT]],
                             input[[myLabels$X1_state]],
                             input[[myLabels$X2_state]],
                             input[[myLabels$X_Interaction]], 
                             VindisNew, 
                             myVind), 
          uiOutput(myLabels$error_Vind))
     }),
     
     output[[myLabels$Vind_UI_hidden]] <- renderUI({
       
       myNT <- as.numeric(input[[myLabels$NT]])   
       
       if(myLabels$Vind_temp %in% names(input)){ 
         myVind         <- matrix(rep(0,(nb.IS*myNT)^2),nb.IS*myNT)      
         newSize        <- ifelse(myNT*nb.IS > length(input[[myLabels$B_temp]]),length(input[[myLabels$B_temp]]),myNT*nb.IS)      
         myVind[1:newSize, 1:newSize]     <- input[[myLabels$Vind_temp]][1:newSize, 1:newSize] 
       }else{ 
         myVind         <- matrix(rep(0,(nb.IS*myNT)^2),nb.IS*myNT) 
         myVind[1:nb.IS, 1:nb.IS]     <- FullModel_VAR$Vind$value[1:nb.IS, 1:nb.IS] 
       }
       myVind[which(is.na(myVind))] <- 0
       
       shinyMatrix::matrixInput(myLabels$Vind, value = myVind, class = "numeric")
     }),
     outputOptions(output, myLabels$Vind_UI_hidden, suspendWhenHidden = FALSE),

     # Ve matrix
     output[[myLabels$Ve_hidden]] <- renderUI({
        shinyMatrix::matrixInput(myLabels$Ve, 
                                value = matrix(c(input[[myLabels$Ve_input]], 0, 0, input[[myLabels$Ve_input]]), 
                                        ncol = as.numeric(input[[myLabels$NT]]),
                                        nrow = as.numeric(input[[myLabels$NT]])), 
                                class = "numeric")
     }),
     outputOptions(output, myLabels$Ve_hidden, suspendWhenHidden = FALSE),
     
     # VG matrix
     output[[myLabels$VG_hidden]] <- renderUI({
       shinyMatrix::matrixInput(myLabels$VG, 
                                value = matrix(c(input[[myLabels$VG_input]], 0, 0, input[[myLabels$VG_input]]), 
                                               ncol = as.numeric(input[[myLabels$NT]]),
                                               nrow = as.numeric(input[[myLabels$NT]])), 
                                class = "numeric")
     }),
     outputOptions(output, myLabels$VG_hidden, suspendWhenHidden = FALSE),
     
          
     ########################
     
     # Switch to the output panel when the app is run
     observe({
       if(input[[myLabels$runButton]] != 0 || input[[myLabels$rerunButton]] != 0){ 
         updateTabsetPanel(session, myLabels$modTabsetPanel, selected = "Outputs")
       }
     }),
     
     myFullModel <- reactive({ 
       
       # if Run button is pressed
       if(input[[myLabels$runButton]] == 0 & input[[myLabels$rerunButton]] == 0)
          return(NULL)
       
       isolate({ 
         
         # browser()
         
         updateCheckboxInput(session, "isRunning", value = TRUE)
         
         # Call app main function
         data <- squid::squidR(input=input, plot=TRUE, module=myModule)
         
         names(data$full_data)    <- outputNames
         names(data$sampled_data) <- outputNames
         
         updateCheckboxInput(session, "isRunning", value = FALSE)
         
         return(data)
         
       })
     }),

     output[[myLabels$plotEnvironment]] <- renderPlot({
          
       data <- myFullModel()
       #   print result graphs
       if(!is.null(data)){
         print(multiplot(data$plots$X1,
                         data$plots$X2,
                         data$plots$X1X2,
                         cols=1))
       }
       
     }),
     
     output[[myLabels$plotPhenotype]] <- renderPlot({
         
       data <- myFullModel()
       #   print result graphs
       if(!is.null(data)){
         print(multiplot(data$plots$totPhen,
                         data$plots$sampPhen,
                         cols=1))
       }
       
     }),
     
     output[[myLabels$plotSamples]] <- renderPlot({
       data <- myFullModel()
       #   print result graphs
       if(!is.null(data)) print(data$plots$sampTime)
     }),
     
     # update the sampling time length for each individual
     output[[myLabels$SampTime]] <- renderText({ 
       FullModel_VAR$NR$max <<- round(input[[myLabels$Tmax]]*(1-input[[myLabels$Vhsi]])) 
     }),

     # Update sampling time checkbox relative to the sampling record checkbox
     observe({
       if(input[[myLabels$ST_ind]])   updateCheckboxInput(session, myLabels$NR_ind,   value = TRUE)
       if(input[[myLabels$ST_trait]]) updateCheckboxInput(session, myLabels$NR_trait, value = TRUE)
       
       if(!input[[myLabels$NR_ind]]   & input[[myLabels$ST_ind]])     updateCheckboxInput(session, myLabels$NR_ind,   value = TRUE)
       if(!input[[myLabels$NR_trait]] & input[[myLabels$ST_trait]])   updateCheckboxInput(session, myLabels$NR_trait, value = TRUE)  
       
       if(input[[myLabels$Vhsi]] > 0){
         updateCheckboxInput(session, myLabels$ST_ind,   value = FALSE)
         updateCheckboxInput(session, myLabels$ST_trait,   value = FALSE)
       }
     }),
     
     output[[myLabels$preview_sampling_design]] <- renderPlot({ 
       
       input[[myLabels$preview_sampling_design_btn]]
       myInput <- list("Sampling_Preview_Tmax"     = input[[myLabels$Tmax]],
                       "Sampling_Preview_NI"       = input[[myLabels$NI]],
                       "Sampling_Preview_Vhsi"     = input[[myLabels$Vhsi]],
                       "Sampling_Preview_NR"       = input[[myLabels$NR]],
                       "Sampling_Preview_ST_ind"   = input[[myLabels$ST_ind]],
                       "Sampling_Preview_ST_trait" = input[[myLabels$ST_trait]],
                       "Sampling_Preview_NR_ind"   = input[[myLabels$NR_ind]],
                       "Sampling_Preview_NR_trait" = input[[myLabels$NR_trait]]
       )
       # Call app main function
       data <- squid::squidR(myInput, module="Sampling_Preview", plot=TRUE)
       print(data$plots$sampTime)
     }),
     
     ######################################################################################
     ################################## ENVIRONMENT #######################################
     ######################################################################################

      # Update environment states
      observe({
        
        # Update X1 environment state
        if(input[[myLabels$X1_sto_state]] || input[[myLabels$X1_lin_state]]|| input[[myLabels$X1_cyc_state]]){
          updateCheckboxInput(session, myLabels$X1_state, value = TRUE)
        }else{
          updateCheckboxInput(session, myLabels$X1_state, value = FALSE)
        }
        
        # Update X2 environment state
        if(input[[myLabels$X2_sto_state]] || input[[myLabels$X2_lin_state]]|| input[[myLabels$X2_cyc_state]]){
          updateCheckboxInput(session, myLabels$X2_state, value = TRUE)
        }else{
          updateCheckboxInput(session, myLabels$X2_state, value = FALSE)
        }

        if(!input[[myLabels$X1_sto_state]]) updateCheckboxInput(session, myLabels$X1_sto_shared, value = TRUE)
        if(!input[[myLabels$X1_lin_state]]) updateCheckboxInput(session, myLabels$X1_lin_shared, value = TRUE)
        if(!input[[myLabels$X1_cyc_state]]) updateCheckboxInput(session, myLabels$X1_cyc_shared, value = TRUE)

        if(!input[[myLabels$X2_sto_state]]) updateCheckboxInput(session, myLabels$X2_sto_shared, value = TRUE)
        if(!input[[myLabels$X2_lin_state]]) updateCheckboxInput(session, myLabels$X2_lin_shared, value = TRUE)
        if(!input[[myLabels$X2_cyc_state]]) updateCheckboxInput(session, myLabels$X2_cyc_shared, value = TRUE)

        # Interaction state
        if(!input[[myLabels$X1_state]] || !input[[myLabels$X2_state]]) updateCheckboxInput(session, myLabels$X_Interaction, value = FALSE)

      }),

      output[[myLabels$X1_plotEnvironment]] <- renderPlot({squid::squidR(input, module=myModule, X_previsualization="X1")}),
      output[[myLabels$X2_plotEnvironment]] <- renderPlot({squid::squidR(input, module=myModule, X_previsualization="X2")}),
     
    ######################################################################################
    ############################### VARIANCES SUMMARY ####################################
    ######################################################################################

    output[[myLabels$variancesTable]] <- renderUI({  
    
      myTable <- data.frame(
        "Variance"    = c("$\\text{Fixed effects}$",
                         paste("$V_{",EQ3$mean1,EQ2$env1,"}$",sep=""),
                         paste("$V_{",EQ3$mean2,EQ2$env2,"}$",sep=""),
                         paste("$V_{",EQ3$mean12,EQ2$env12,"}$",sep=""),
                         "$\\text{Random effects}$",
                         paste("$V_",NOT$devI,"$",sep=""),
                         paste("$V_{",EQ3$dev1,EQ2$env1,"}$",sep=""),
                         paste("$V_{",EQ3$dev2,EQ2$env2,"}$",sep=""),
                         paste("$V_{",EQ3$dev12,EQ2$env12,"}$",sep=""),
#                          paste("$2COV_{",NOT$devI,",",EQ3$dev1,EQ2$env1,"}$",sep=""),
#                          paste("$2COV_{",NOT$devI,",",EQ3$dev2,EQ2$env2,"}$",sep=""),
#                          paste("$2COV_{",NOT$devI,",",EQ3$dev12,EQ2$env12,"}$",sep=""),
#                          paste("$2COV_{",EQ3$dev1,EQ2$env1,",",EQ3$dev2,EQ2$env2,"}$",sep=""),
#                          paste("$2COV_{",EQ3$dev1,EQ2$env1,",",EQ3$dev12,EQ2$env12,"}$",sep=""),
#                          paste("$2COV_{",EQ3$dev2,EQ2$env2,",",EQ3$dev12,EQ2$env12,"}$",sep=""),
                         paste("$V_",NOT$groupV,"$",sep=""),
                         paste("$V_",NOT$residualUpper,"$",sep=""),
                         paste("$V_",NOT$total,"$",sep="")
                        ),

      "Explanation" = c(" ",
                        paste("Population-average response to an environmental effect $",EQ2$env1,"$ variance",sep=""),
                        paste("Population-average response to an environmental effect $",EQ2$env2,"$ variance",sep=""),
                        paste("Population-average response interaction to two environmental effect $",EQ2$env1,"$ and $",EQ2$env2,"$ variance",sep=""),
                        " ",
                        "Individual-specific deviations (random intercepts) variance",
                        paste("Individual-specific response to an environmental effect $",EQ2$env1,"$ (random slopes) variance",sep=""),
                        paste("Individual-specific response to an environmental effect $",EQ2$env2,"$ (random slopes) variance",sep=""),
                        paste("Individual-specific response interaction to two environmental effects $",EQ2$env1,"$ and $",EQ2$env2,"$ (random slopes) variance",sep=""),
#                         paste("Covariance between random intercepts and random-slopes in response to an environmental effect $",EQ2$env1,"$.",sep=""),
#                         paste("Covariance between random intercepts and random-slopes in response to an environmental effect $",EQ2$env2,"$.",sep=""),
#                         paste("Covariance between random intercepts and individual-specific response interaction 
#                               to two environmental effects ($",EQ2$env1,"$, $",EQ2$env2,"$) (random slopes).",sep=""),
#                         paste("Covariance between random-slopes in response to an environmental effect $",EQ2$env1,"$ 
#                               and random-slopes in response to an environmental effect $",EQ2$env2,"$.",sep=""),
#                         paste("Covariance between random -slopes in response to an environmental effect $",EQ2$env1,"$ 
#                               and individual-specific response interaction to two environmental effects ($",EQ2$env1,"$, $",EQ2$env2,"$).",sep=""),
#                         paste("Covariance between random -slopes in response to an environmental effect $",EQ2$env2,"$ 
#                               and individual-specific response interaction to two environmental effects ($",EQ2$env1,"$, $",EQ2$env2,"$).",sep=""),
#                         
                        "Higher-level grouping variance (clusters, groups, families etc.)", 
                        "Residual variance", 
                        "Total phenotypic variance"
                      ),
        stringsAsFactors = FALSE
      )

      mySummary <- SVRGetSummaryVariances(input,
      																		myLabels,
      																		0,
      																		nb.IS,
      																		NOT$trait.1)
      myTable   <- cbind(myTable,mySummary)

      if(input[[myLabels$NT]] > 1){
        mySummary <- SVRGetSummaryVariances(input,
        																		myLabels,
        																		nb.IS,
        																		nb.IS,
        																		NOT$trait.2)
        myTable   <- cbind(myTable,mySummary)
        myTable   <- subset(myTable, Trait.y != "0 (0%)" | Trait.z != "0 (0%)")
        myTable   <- rbind(c("Variance", "Explanation", "Trait y", "Trait z"),myTable)
      }else{
        myTable   <- subset(myTable, Trait.y != "0 (0%)")
        myTable   <- rbind(c("Variance", "Explanation", "Trait y"),myTable)
      }

      getTable(myTable, header=TRUE)
    }),

    ######################################################################################
    ################################## ERROR MANAGER #####################################
    ######################################################################################

     isError <- reactive({
       
      FullModel_VAR$NR$max    <<- round(input[[myLabels$Tmax]]*(1-input[[myLabels$Vhsi]])) 
      FullModel_VAR$NG$modulo <<- input[[myLabels$NI]] 
      
      if(!testInput(input[[myLabels$Tmax]], FullModel_VAR$Tmax, TRUE, FALSE)                  ||
         !testInput(input[[myLabels$NP]], FullModel_VAR$NP, TRUE, FALSE)                      ||            
         !testInput(input[[myLabels$X1_sto_V]], FullModel_VAR$stoV, FALSE, FALSE)             ||
         !testInput(input[[myLabels$X2_sto_V]], FullModel_VAR$stoV, FALSE, FALSE)             ||
         !testInput(input[[myLabels$X1_sto_corr]], FullModel_VAR$stoCorr, FALSE, FALSE)       || 
         !testInput(input[[myLabels$X2_sto_corr]], FullModel_VAR$stoCorr, FALSE, FALSE)       || 
         !testInput(input[[myLabels$NI]], FullModel_VAR$NI, TRUE, FALSE, (input[[myLabels$NI]]%%input[[myLabels$NG]] != 0)) ||
         !testInput(input[[myLabels$Ve_input]], FullModel_VAR$Ve, FALSE, FALSE)                   ||
         !testInput(input[[myLabels$VG_input]], FullModel_VAR$VG, FALSE, FALSE)                     ||
         !testInput(input[[myLabels$NG]], FullModel_VAR$NG, TRUE, FALSE, (input[[myLabels$NI]]%%input[[myLabels$NG]] != 0)) ||
         !testInput(input[[myLabels$NR]], FullModel_VAR$NR, TRUE, FALSE)                      ||
         !testInputBMatrix(input[[myLabels$B]] , FullModel_VAR$B, FALSE)                      ||
         !testInputVindMatrix(input[[myLabels$Vind]] , FullModel_VAR$Vind, FALSE)){
        return(TRUE)
      } 
       return(FALSE)
    }),
     
     # Display error message
     output[[myLabels$runButtonError]] <- renderUI({
       if(isError()){
         error_msg(FullModel_VAR$Run$errorTxt)
       }else{
         NULL
       }
     }),
     output[[myLabels$rerunButtonError]] <- renderUI({
      if(isError()){
        error_msg(FullModel_VAR$ReRun$errorTxt)
      }else{
        NULL
      }
    }),
     
     ######### Manage errors #########
     # display error message and disable button if so
     observe({
       if(isError()){ 
         disableActionButton(myLabels$runButton, session, "true")
         disableActionButton(myLabels$rerunButton, session, "true")
       }else{
         disableActionButton(myLabels$runButton, session, "false")
         disableActionButton(myLabels$rerunButton, session, "false")
       }
     }),
    
     output[[myLabels$error_Tmax]]            <- renderUI({testInput(input[[myLabels$Tmax]], FullModel_VAR$Tmax, TRUE, TRUE)}),
     output[[myLabels$error_NP]]              <- renderUI({testInput(input[[myLabels$NP]], FullModel_VAR$NP, TRUE, TRUE)}),     
     output[[myLabels$error_X1_sto_V]]        <- renderUI({testInput(input[[myLabels$X1_sto_V]], FullModel_VAR$stoV, FALSE, TRUE)}),
     output[[myLabels$error_X2_sto_V]]        <- renderUI({testInput(input[[myLabels$X2_sto_V]], FullModel_VAR$stoV, FALSE, TRUE)}),
     output[[myLabels$error_X1_sto_corr]]     <- renderUI({testInput(input[[myLabels$X1_sto_corr]], FullModel_VAR$stoCorr, FALSE, TRUE)}),
     output[[myLabels$error_X2_sto_corr]]     <- renderUI({testInput(input[[myLabels$X2_sto_corr]], FullModel_VAR$stoCorr, FALSE, TRUE)}),
     output[[myLabels$error_NI]]              <- renderUI({testInput(input[[myLabels$NI]], FullModel_VAR$NI, TRUE, TRUE, (input[[myLabels$NI]]%%input[[myLabels$NG]] != 0))}),
     output[[myLabels$error_Ve]]              <- renderUI({testInput(input[[myLabels$Ve_input]], FullModel_VAR$Ve, FALSE, TRUE)}),
     output[[myLabels$error_NG]]              <- renderUI({testInput(input[[myLabels$NG]], FullModel_VAR$NG, TRUE, TRUE, (input[[myLabels$NI]]%%input[[myLabels$NG]] != 0))}),
     output[[myLabels$error_VG]]              <- renderUI({testInput(input[[myLabels$VG_input]], FullModel_VAR$VG, FALSE, TRUE)}),
     output[[myLabels$error_NR]] <- renderUI({
          input[[myLabels$Tmax]];input[[myLabels$Vhsi]];
          testInput(input[[myLabels$NR]], FullModel_VAR$NR, TRUE, TRUE) 
       }),
     output[[myLabels$error_B]]               <- renderUI({testInputBMatrix(input[[myLabels$B]] , FullModel_VAR$B, TRUE)}),
     output[[myLabels$error_Vind]]            <- renderUI({testInputVindMatrix(input[[myLabels$Vind]], FullModel_VAR$Vind, TRUE)}),

    ######################################################################################
    ################################## DOWNLOAD DATA #####################################
    ######################################################################################

    output[[myLabels$download_sampled]] <- downloadHandler(
      filename = function() {
        paste0('sampled_data_', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(myFullModel()$sampled_data, con)
      }
    ),

    output[[myLabels$download_raw]] <- downloadHandler(
      filename = function() {
        paste0('raw_data_', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(myFullModel()$full_data, con)
      }
    ),

		output[[myLabels$download_Rcode]] <- downloadHandler(
			filename = function() {
				paste0('Rcode_', Sys.Date(), '.R')
			},
			content = function(con) {
				writeLines(SVRcreateRcode(input, myLabels), con)
			}
		),

    output[[myLabels$Data_Description_Table]] <- renderUI({

      myTable <- data.frame(
        "Output data variable"= c("Output data variable",
                                  "Replicate",
                                  "Individual",
                                  "Group",
                                  "Individual_Trait",
                                  "Trait",
                                  "Time",
                                  "Phenotype",
                                  "B0",
                                  "B1",
                                  "B2",
                                  "B12",
                                  "I",
                                  "S1",
                                  "S2",
                                  "S12",
                                  "X1",
                                  "X2",
                                  "X1X2",
                                  "G",
                                  "e"),
        "Mathematical symbol" = c("Mathematical symbol",
                                  "",
                                  paste0("$",NOT$ind,"$"),
                                  paste0("$",NOT$group,"$"),
                                  "",
                                  paste0("$",NOT$trait.1,"$ (1) and $",NOT$trait.2,"$ (2)"),
                                  paste0("$",NOT$time,"$"),
                                  paste0("$",EQ$phen.1,"$ and $",EQ$phen.2,"$"),
                                  paste0("$",EQ3$mean0,"$"),
                                  paste0("$",EQ3$mean1,"$"),
                                  paste0("$",EQ3$mean2,"$"),
                                  paste0("$",EQ3$mean12,"$"),
                                  paste0("$",EQ1$dev0,"$"),
                                  paste0("$",EQ1$dev1,"$"),
                                  paste0("$",EQ1$dev2,"$"),
                                  paste0("$",EQ1$dev12,"$"),
                                  paste0("$",EQ1$env1,"$"),
                                  paste0("$",EQ1$env2,"$"),
                                  paste0("$",EQ1$env12,"$"),
                                  paste0("$",EQ1$group,"$"),
                                  paste0("$",EQ1$error,"$")),
        "Description" = c("Description",
                          "Replicate identifier.",
                          "Individual identifier.",
                          "Higher-level grouping identifier.",
                          "Identifier fo each individual/trait combination.",
                          "Trait identifier.",
                          "Time step values.",
                          "Individual phenotype.",
                          "Population phenotypic mean.",
                          paste0("Population mean response to environmental influences $",EQ2$env1,"$."),
                          paste0("Population mean response to environmental influences $",EQ2$env2,"$."),
                          paste0("Population mean response to environmental influences $",EQ2$env12,"$."),
                          paste0("Individual-specific deviations (random-intercepts) from population phenotypic mean $",EQ3$mean0,"$."),
                          paste0("Individual-specific response to environmental influence $",EQ2$env1,"$ (random-slope)."),
                          paste0("Individual-specific response to environmental influence $",EQ2$env2,"$ (random-slope)."),
                          paste0("Individual-specific response to environmental influence $",EQ2$env12,"$ (random-slope)."),
                          paste0("Environmental gradient ($",EQ2$env1,"$)."),
                          paste0("Environmental gradient ($",EQ2$env2,"$)."),
                          paste0("Environmental gradient ($",EQ2$env12,"$)."),
                          paste0("Higher-level grouping value."),
                          paste0("Residual."))
      )  
      
      return(getTable(myTable, header=TRUE))
      
    })#,

#     observe({
#       
#       inFile <- input[[myLabels$load_inputs]]
#       if (is.null(inFile))
#         return(NULL)
#       
#       load(inFile$datapath)
#       isolate({
#         
#         updateNumericInput(session, inputId = Tmax, value = saveFile[[Tmax]])
#         updateNumericInput(session, inputId = NP,   value = saveFile[[NP]])
#         updateNumericInput(session, inputId = NI,   value = saveFile[[NI]])
#         updateNumericInput(session, inputId = NG,   value = saveFile[[NG]])
#         updateSelectInput(session,  inputId = NT,   selected = saveFile[[NT]])
# 
#         # Environment X1
#           # Stochastic environment
#           updateCheckboxInput(session, inputId = X1_sto_state,  value = saveFile[[X1_sto_state]])
#           updateNumericInput(session,  inputId = X1_sto_V,      value = saveFile[[X1_sto_V]])
#           updateCheckboxInput(session, inputId = X1_sto_autocor_state,  value = saveFile[[X1_sto_autocor_state]])
#           updateNumericInput(session,  inputId = X1_sto_corr,   value = saveFile[[X1_sto_corr]])
#           updateCheckboxInput(session, inputId = X1_sto_shared, value = saveFile[[X1_sto_shared]])
#           
#           # Linear environment
#           updateCheckboxInput(session, inputId = X1_lin_state,     value = saveFile[[X1_lin_state]])
#           updateNumericInput(session,  inputId = X1_lin_Intercept, value = saveFile[[X1_lin_Intercept]])
#           updateNumericInput(session,  inputId = X1_lin_Slope,     value = saveFile[[X1_lin_Slope]])
#           updateCheckboxInput(session, inputId = X1_lin_shared,    value = saveFile[[X1_lin_shared]])
#           updateNumericInput(session,  inputId = X1_lin_V,         value = saveFile[[X1_lin_V]])
#           
#           # Cyclic environment
#           updateCheckboxInput(session, inputId = X1_cyc_state, value = saveFile[[X1_cyc_state]])
#           updateNumericInput(session,  inputId = X1_cyc_Amplitude, value = saveFile[[X1_cyc_Amplitude]])
#           updateNumericInput(session,  inputId = X1_cyc_Period, value = saveFile[[X1_cyc_Period]])
#           updateNumericInput(session,  inputId = X1_cyc_Hshift, value = saveFile[[X1_cyc_Hshift]])
#           updateNumericInput(session,  inputId = X1_cyc_Vshift, value = saveFile[[X1_cyc_Vshift]])
#           updateCheckboxInput(session, inputId = X1_cyc_shared, value = saveFile[[X1_cyc_shared]])
#           updateNumericInput(session,  inputId = X1_cyc_V,      value = saveFile[[X1_cyc_V]])
#           
#         # Environment X2
#           # Stochastic environment
#           updateCheckboxInput(session, inputId = X2_sto_state,  value = saveFile[[X2_sto_state]])
#           updateNumericInput(session,  inputId = X2_sto_V,      value = saveFile[[X2_sto_V]])
#           updateCheckboxInput(session, inputId = X2_sto_autocor_state,  value = saveFile[[X2_sto_autocor_state]])
#           updateNumericInput(session,  inputId = X2_sto_corr,   value = saveFile[[X2_sto_corr]])
#           updateCheckboxInput(session, inputId = X2_sto_shared, value = saveFile[[X2_sto_shared]])
#           
#           # Linear environment
#           updateCheckboxInput(session, inputId = X2_lin_state,     value = saveFile[[X2_lin_state]])
#           updateNumericInput(session,  inputId = X2_lin_Intercept, value = saveFile[[X2_lin_Intercept]])
#           updateNumericInput(session,  inputId = X2_lin_Slope,     value = saveFile[[X2_lin_Slope]])
#           updateCheckboxInput(session, inputId = X2_lin_shared,    value = saveFile[[X2_lin_shared]])
#           updateNumericInput(session,  inputId = X2_lin_V,         value = saveFile[[X2_lin_V]])
#           
#           # Cyclic environment
#           updateCheckboxInput(session, inputId = X2_cyc_state, value = saveFile[[X2_cyc_state]])
#           updateNumericInput(session,  inputId = X2_cyc_Amplitude, value = saveFile[[X2_cyc_Amplitude]])
#           updateNumericInput(session,  inputId = X2_cyc_Period, value = saveFile[[X2_cyc_Period]])
#           updateNumericInput(session,  inputId = X2_cyc_Hshift, value = saveFile[[X2_cyc_Hshift]])
#           updateNumericInput(session,  inputId = X2_cyc_Vshift, value = saveFile[[X2_cyc_Vshift]])
#           updateCheckboxInput(session, inputId = X2_cyc_shared, value = saveFile[[X2_cyc_shared]])
#           updateNumericInput(session,  inputId = X2_cyc_V,      value = saveFile[[X2_cyc_V]])
#         
#         # environment interaction
#         updateCheckboxInput(session, inputId = X_Interaction, value = saveFile[[X_Interaction]]) ## doesn't work
# 
#         updateNumericInput(session,  inputId = Ve, value = saveFile[[Ve]])
#         updateNumericInput(session,  inputId = VG,  value = saveFile[[VG]])
#         
#         updateNumericInput(session,  inputId = NR,  value = saveFile[[NR]])
#         updateSliderInput(session,   inputId = Vhsi, value = saveFile[[Vhsi]])
#         updateCheckboxInput(session, inputId = NR_ind,    value = saveFile[[NR_ind]])
#         updateCheckboxInput(session, inputId = NR_trait,  value = saveFile[[NR_trait]])
#         updateCheckboxInput(session, inputId = ST_ind,   value = saveFile[[ST_ind]])
#         updateCheckboxInput(session, inputId = ST_trait, value = saveFile[[ST_trait]])
#         
#       })
#       
#     }),  
# 
#     output[[myLabels$save_inputs]] <- downloadHandler(
#       filename = function() {
#         paste('inputs-', Sys.Date(), '.RData', sep="")
#       },
#       content = function(con) {
#         saveFile <- reactiveValuesToList(input)
#         save(saveFile, file = con)
#       }
#     )

  )) # End return
}