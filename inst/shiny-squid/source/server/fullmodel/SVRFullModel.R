#Server functions for the full model
SVRFullModel <- function(myModule, input, output, session){
  
  myEquations        <- paste(myModule, "myEquations", sep="_")
  
  Tmax               <- paste(myModule, "Tmax", sep="_")
  NT                 <- paste(myModule, "NT", sep="_")
  NP                 <- paste(myModule, "NP", sep="_")
  NI                 <- paste(myModule, "NI", sep="_")
  Ve                 <- paste(myModule, "Ve", sep="_")
  NG                 <- paste(myModule, "NG", sep="_")
  VG                 <- paste(myModule, "VG", sep="_")
  NR                 <- paste(myModule, "NR", sep="_")
  Vhsi                <- paste(myModule, "Vhsi", sep="_")

  B                  <- paste(myModule, "B", sep="_")
  B_temp             <- paste(myModule, "B_temp", sep="_")
  B_UI               <- paste(myModule, "B_UI", sep="_")
  error_B            <- paste(myModule, "error_B", sep="_") 
  B_UI_hidden        <- paste(myModule, "B_UI_hidden", sep="_")
  
  Vind               <- paste(myModule, "Vind", sep="_")
  Vind_temp          <- paste(myModule, "Vind_temp", sep="_")
  Vind_UI            <- paste(myModule, "Vind_UI", sep="_")
  error_Vind         <- paste(myModule, "error_Vind", sep="_") 
  Vind_UI_hidden     <- paste(myModule, "Vind_UI_hidden", sep="_")
  
  X1_state           <- paste(myModule, "X1_state", sep="_")
  X1_sto_state       <- paste(myModule, "X1_sto_state", sep="_")
  X1_sto_shared      <- paste(myModule, "X1_sto_shared", sep="_")
  X1_sto_V           <- paste(myModule, "X1_sto_V", sep="_")
  X1_sto_autocor_state <- paste(myModule, "X1_sto_autocor_state", sep="_")
  X1_sto_corr        <- paste(myModule, "X1_sto_corr", sep="_")
  X1_lin_state       <- paste(myModule, "X1_lin_state", sep="_")
  X1_lin_intercept   <- paste(myModule, "X1_lin_intercept", sep="_")
  X1_lin_slope       <- paste(myModule, "X1_lin_slope", sep="_")
  X1_lin_shared      <- paste(myModule, "X1_lin_shared", sep="_")
  X1_lin_V           <- paste(myModule, "X1_lin_V", sep="_")
  X1_cyc_state       <- paste(myModule, "X1_cyc_state", sep="_")
  X1_cyc_shared      <- paste(myModule, "X1_cyc_shared", sep="_")
  X1_cyc_amplitude   <- paste(myModule, "X1_cyc_amplitude", sep="_")
  X1_cyc_period      <- paste(myModule, "X1_cyc_period", sep="_")
  X1_cyc_Hshift      <- paste(myModule, "X1_cyc_Hshift", sep="_")
  X1_cyc_Vshift      <- paste(myModule, "X1_cyc_Vshift", sep="_")
  X1_cyc_V           <- paste(myModule, "X1_cyc_V", sep="_")
  X1_plotEnvironment <- paste(myModule, "X1_plotEnvironment", sep="_")
  
  X2_state           <- paste(myModule, "X2_state", sep="_")
  X2_sto_state       <- paste(myModule, "X2_sto_state", sep="_")
  X2_sto_shared      <- paste(myModule, "X2_sto_shared", sep="_")
  X2_sto_V           <- paste(myModule, "X2_sto_V", sep="_")
  X2_sto_autocor_state <- paste(myModule, "X2_sto_autocor_state", sep="_")
  X2_sto_corr        <- paste(myModule, "X2_sto_corr", sep="_")
  X2_lin_state       <- paste(myModule, "X2_lin_state", sep="_")
  X2_lin_intercept   <- paste(myModule, "X2_lin_intercept", sep="_")
  X2_lin_slope       <- paste(myModule, "X2_lin_slope", sep="_")
  X2_lin_shared      <- paste(myModule, "X2_lin_shared", sep="_")
  X2_lin_V           <- paste(myModule, "X2_lin_V", sep="_")
  X2_cyc_state       <- paste(myModule, "X2_cyc_state", sep="_")
  X2_cyc_shared      <- paste(myModule, "X2_cyc_shared", sep="_")
  X2_cyc_amplitude   <- paste(myModule, "X2_cyc_amplitude", sep="_")
  X2_cyc_period      <- paste(myModule, "X2_cyc_period", sep="_")
  X2_cyc_Hshift      <- paste(myModule, "X2_cyc_Hshift", sep="_")
  X2_cyc_Vshift      <- paste(myModule, "X2_cyc_Vshift", sep="_")
  X2_cyc_V           <- paste(myModule, "X2_cyc_V", sep="_")
  X2_plotEnvironment <- paste(myModule, "X2_plotEnvironment", sep="_")
  
  X_Interaction      <- paste(myModule, "X_Interaction", sep="_")
  
  runButton          <- paste(myModule, "runButton", sep="_")
  runButtonError     <- paste(myModule, "runButtonError", sep="_")
  rerunButton        <- paste(myModule, "rerunButton", sep="_")
  rerunButtonError   <- paste(myModule, "rerunButtonError", sep="_")
  
  plotEnvironment    <- paste(myModule, "plotEnvironment", sep="_")
  plotPhenotype      <- paste(myModule, "plotPhenotype", sep="_")
  plotSamples        <- paste(myModule, "plotSamples", sep="_")
  
  variancesTable     <- paste(myModule, "variancesTable", sep="_")
  
  PB                 <- paste(myModule, "PB", sep="_")
  
  modTabsetPanel     <- paste(myModule, "TabsetPanel", sep="_")
  
  SampTime           <- paste(myModule, "SampTime", sep="_")
  
  NR_ind          <- paste(myModule, "NR_ind", sep="_")
  NR_trait        <- paste(myModule, "NR_trait", sep="_")
  ST_ind          <- paste(myModule, "ST_ind", sep="_")
  ST_trait        <- paste(myModule, "ST_trait", sep="_")
  preview_sampling_design     <- paste(myModule, "preview_sampling_design", sep="_")
  preview_sampling_design_btn <- paste(myModule, "preview_sampling_design_btn", sep="_")
  
  download_sampled      <- paste(myModule, "download_sampled", sep="_")
  download_raw          <- paste(myModule, "download_raw", sep="_")
  save_inputs           <- paste(myModule, "save_inputs", sep="_")
  load_inputs           <- paste(myModule, "load_inputs", sep="_")
  
  error_Tmax            <- paste(myModule, "error_Tmax", sep="_")
  error_NP              <- paste(myModule, "error_NP", sep="_")    
  error_X1_sto_V        <- paste(myModule, "error_X1_sto_V", sep="_")
  error_X2_sto_V        <- paste(myModule, "error_X2_sto_V", sep="_")
  error_X1_sto_corr     <- paste(myModule, "error_X1_sto_corr", sep="_")
  error_X2_sto_corr     <- paste(myModule, "error_X2_sto_corr", sep="_")
  error_NI              <- paste(myModule, "error_NI", sep="_")
  error_NG              <- paste(myModule, "error_NG", sep="_")
  error_Ve             <- paste(myModule, "error_Ve", sep="_")
  error_VG              <- paste(myModule, "error_VG", sep="_")
  error_NR              <- paste(myModule, "error_NR", sep="_")
  error_B               <- paste(myModule, "error_B", sep="_")
  error_Vind            <- paste(myModule, "error_Vind", sep="_")
  
  Data_Description_Table<- paste(myModule, "Data_Description_Table", sep="_")
  
  loader             <- paste(myModule, "loader", sep="_")
  
  return(c(
    
     # Equation  trait 1
     output[[myEquations]] <- renderUI({         
        SVRDispayModelEquation(myModule, input)    
      }),
          
     output[[B_UI]]<- renderUI({ 
        
        if(B_temp %in% names(input)){
          BisNew <- FALSE
          myB    <- input[[B_temp]]
        }else{
          BisNew <- TRUE
          myB    <- NULL
        }      
        
        list(SVRGetBMatrix(B_temp,
                           input[[NT]],
                           input[[X1_state]],
                           input[[X2_state]],
                           input[[X_Interaction]],
                           BisNew,
                           myB), 
             uiOutput(error_B))
     }),
      
     output[[B_UI_hidden]] <- renderUI({
        
        myNT <- as.numeric(input[[NT]])    
        
        if(B_temp %in% names(input)){ 
          myB            <- matrix(rep(0,myNT*nb.IS),1)      
          newSize        <- ifelse(myNT*nb.IS > length(input[[B_temp]]),length(input[[B_temp]]),myNT*nb.IS)      
          myB[1:newSize] <- input[[B_temp]][1:newSize]   
        }else{ 
          myB            <- matrix(rep(0,myNT*nb.IS),1)            
          myB[1:nb.IS]   <- FullModel_VAR$B$value[1:nb.IS] 
        }
        
        myB[which(is.na(myB))] <- 0
        
        matrixInput2(B, "",data.frame(myB))
      }),
     outputOptions(output, B_UI_hidden, suspendWhenHidden = FALSE),
     
     output[[Vind_UI]] <- renderUI({
       
       if(Vind_temp %in% names(input)){
         VindisNew <- FALSE
         myVind    <- input[[Vind_temp]]
       }else{
         VindisNew <- TRUE
         myVind    <- NULL
       }    
       
       list(SVRGetVindMatrix(Vind_temp,
                             input[[NT]],
                             input[[X1_state]],
                             input[[X2_state]],
                             input[[X_Interaction]], 
                             VindisNew, 
                             myVind), 
          uiOutput(error_Vind))
     }),
     
     output[[Vind_UI_hidden]] <- renderUI({
       
       myNT <- as.numeric(input[[NT]])   
       
       if(Vind_temp %in% names(input)){ 
         myVind         <- matrix(rep(0,(nb.IS*myNT)^2),nb.IS*myNT)      
         newSize        <- ifelse(myNT*nb.IS > length(input[[B_temp]]),length(input[[B_temp]]),myNT*nb.IS)      
         myVind[1:newSize, 1:newSize]     <- input[[Vind_temp]][1:newSize, 1:newSize] 
       }else{ 
         myVind         <- matrix(rep(0,(nb.IS*myNT)^2),nb.IS*myNT) 
         myVind[1:nb.IS, 1:nb.IS]     <- FullModel_VAR$Vind$value[1:nb.IS, 1:nb.IS] 
       }
       
       myVind[which(is.na(myVind))] <- 0
       
       matrixInput2(Vind, "",data.frame(myVind))
     }),
     outputOptions(output, Vind_UI_hidden, suspendWhenHidden = FALSE),
     
     ########################
     
     # Switch to the output panel when the app is runned
     observe({
       if(input[[runButton]] != 0 || input[[rerunButton]] != 0){ 
         updateTabsetPanel(session, modTabsetPanel, selected = "Outputs")
       }
     }),
     
     myFullModel <- reactive({ 
       
       # if Run button is pressed
       if(input[[runButton]] == 0 & input[[rerunButton]] == 0)
          return(NULL)
       
       isolate({ 
         
         updateCheckboxInput(session, "isRunning", value = TRUE)
         
         # Call app main function
         data <- squid::squidR(input=input, plot=TRUE, module=myModule) 
         
         names(data$full_data)    <- outputNames
         names(data$sampled_data) <- outputNames
         
         updateCheckboxInput(session, "isRunning", value = FALSE)
         
         return(data)
         
       })              
     }),

     output[[plotEnvironment]] <- renderPlot({
          
       data <- myFullModel()
       #   print result graphs 
       if(!is.null(data)){
         print(multiplot(data$plots$X1,                    
                         data$plots$X2,
                         data$plots$X1X2,
                         cols=1))
       }
       
     }),
     
     output[[plotPhenotype]] <- renderPlot({
         
       data <- myFullModel() 
       #   print result graphs 
       if(!is.null(data)){
         print(multiplot(data$plots$totPhen,                    
                         data$plots$sampPhen,                    
                         cols=1))
       }
       
     }),
     
     output[[plotSamples]] <- renderPlot({
       data <- myFullModel()
       #   print result graphs 
       if(!is.null(data)) print(data$plots$sampTime)
     }),
     
     # update the sampling time length for each individual    
     output[[SampTime]] <- renderText({ 
       FullModel_VAR$NR$max <<- round(input[[Tmax]]*(1-input[[Vhsi]])) 
     }),

     # Update sampling time checkbox relative to the sampling record checkbox
     observe({
       if(input[[ST_ind]])   updateCheckboxInput(session, NR_ind,   value = TRUE)
       if(input[[ST_trait]]) updateCheckboxInput(session, NR_trait, value = TRUE)
       
       if(!input[[NR_ind]]   & input[[ST_ind]])     updateCheckboxInput(session, NR_ind,   value = TRUE)
       if(!input[[NR_trait]] & input[[ST_trait]])   updateCheckboxInput(session, NR_trait, value = TRUE)  
       
       if(input[[Vhsi]] > 0){
         updateCheckboxInput(session, ST_ind,   value = FALSE)
         updateCheckboxInput(session, ST_trait,   value = FALSE)
       }
     }),
     
     output[[preview_sampling_design]] <- renderPlot({ 
       
       input[[preview_sampling_design_btn]]
       myInput <- list("Sampling_Preview_Tmax"     = input[[Tmax]],
                       "Sampling_Preview_NI"       = input[[NI]],
                       "Sampling_Preview_Vhsi"     = input[[Vhsi]],
                       "Sampling_Preview_NR"       = input[[NR]],
                       "Sampling_Preview_ST_ind"   = input[[ST_ind]],
                       "Sampling_Preview_ST_trait" = input[[ST_trait]],
                       "Sampling_Preview_NR_ind"   = input[[NR_ind]],
                       "Sampling_Preview_NR_trait" = input[[NR_trait]]
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
         ifelse(input[[X1_sto_state]] || input[[X1_lin_state]]|| input[[X1_cyc_state]], 
                updateCheckboxInput(session, X1_state, value = TRUE), 
                updateCheckboxInput(session, X1_state, value = FALSE))
        
        # Update X2 environment state
        ifelse(input[[X2_sto_state]] || input[[X2_lin_state]]|| input[[X2_cyc_state]], 
               updateCheckboxInput(session, X2_state, value = TRUE), 
               updateCheckboxInput(session, X2_state, value = FALSE))
        
        if(!input[[X1_sto_state]]) updateCheckboxInput(session, X1_sto_shared, value = TRUE)
        if(!input[[X1_lin_state]]) updateCheckboxInput(session, X1_lin_shared, value = TRUE)
        if(!input[[X1_cyc_state]]) updateCheckboxInput(session, X1_cyc_shared, value = TRUE)
        
        if(!input[[X2_sto_state]]) updateCheckboxInput(session, X2_sto_shared, value = TRUE)  
        if(!input[[X2_lin_state]]) updateCheckboxInput(session, X2_lin_shared, value = TRUE)
        if(!input[[X2_cyc_state]]) updateCheckboxInput(session, X2_cyc_shared, value = TRUE)
        
        # Interaction state
        if(!input[[X1_state]] || !input[[X2_state]]) updateCheckboxInput(session, X_Interaction, value = FALSE)

      }),

      output[[X1_plotEnvironment]] <- renderPlot({squid::squidR(input, module=myModule, X_previsualization="X1")}),
      output[[X2_plotEnvironment]] <- renderPlot({squid::squidR(input, module=myModule, X_previsualization="X2")}),
     
    ######################################################################################
    ############################### VARIANCES SUMMARY ####################################
    ######################################################################################

    output[[variancesTable]] <- renderUI({  
    
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

      mySummary <- SVRGetSummaryVariances(input,B,Vind,Ve,VG,NT,0,nb.IS,NOT$trait.1,X1_state,X2_state,X_Interaction)
      myTable   <- cbind(myTable,mySummary)

      if(input[[NT]] > 1){
        mySummary <- SVRGetSummaryVariances(input,B,Vind,Ve,VG,NT,nb.IS,nb.IS,NOT$trait.2,X1_state,X2_state,X_Interaction)
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
       
      FullModel_VAR$NR$max    <<- round(input[[Tmax]]*(1-input[[Vhsi]])) 
      FullModel_VAR$NG$modulo <<- input[[NI]] 
      
      if(!testInput(input[[Tmax]], FullModel_VAR$Tmax, TRUE, FALSE)                  ||
         !testInput(input[[NP]], FullModel_VAR$NP, TRUE, FALSE)                      ||            
         !testInput(input[[X1_sto_V]], FullModel_VAR$stoV, FALSE, FALSE)             ||
         !testInput(input[[X2_sto_V]], FullModel_VAR$stoV, FALSE, FALSE)             ||
         !testInput(input[[X1_sto_corr]], FullModel_VAR$stoCorr, FALSE, FALSE)       || 
         !testInput(input[[X2_sto_corr]], FullModel_VAR$stoCorr, FALSE, FALSE)       || 
         !testInput(input[[NI]], FullModel_VAR$NI, TRUE, FALSE, (input[[NI]]%%input[[NG]] != 0)) ||
         !testInput(input[[Ve]], FullModel_VAR$Ve, FALSE, FALSE)                   ||
         !testInput(input[[VG]], FullModel_VAR$VG, FALSE, FALSE)                     ||
         !testInput(input[[NG]], FullModel_VAR$NG, TRUE, FALSE, (input[[NI]]%%input[[NG]] != 0)) ||
         !testInput(input[[NR]], FullModel_VAR$NR, TRUE, FALSE)                      ||
         !testInputBMatrix(input[[B]] , FullModel_VAR$B, FALSE)                      ||
         !testInputVindMatrix(input[[Vind]] , FullModel_VAR$Vind, FALSE)){
        return(TRUE)
      } 
       return(FALSE)
    }),
     
     # Display error message
     output[[runButtonError]] <- renderUI({
       if(isError()){
         error_msg(FullModel_VAR$Run$errorTxt)
       }else{
         NULL
       }
     }),
     output[[rerunButtonError]] <- renderUI({
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
         disableActionButton(runButton, session, "true")
         disableActionButton(rerunButton, session, "true")
       }else{
         disableActionButton(runButton, session, "false")
         disableActionButton(rerunButton, session, "false")
       }
     }),
    
     output[[error_Tmax]]            <- renderUI({testInput(input[[Tmax]], FullModel_VAR$Tmax, TRUE, TRUE)}),
     output[[error_NP]]              <- renderUI({testInput(input[[NP]], FullModel_VAR$NP, TRUE, TRUE)}),     
     output[[error_X1_sto_V]]        <- renderUI({testInput(input[[X1_sto_V]], FullModel_VAR$stoV, FALSE, TRUE)}),
     output[[error_X2_sto_V]]        <- renderUI({testInput(input[[X2_sto_V]], FullModel_VAR$stoV, FALSE, TRUE)}),
     output[[error_X1_sto_corr]]     <- renderUI({testInput(input[[X1_sto_corr]], FullModel_VAR$stoCorr, FALSE, TRUE)}),
     output[[error_X2_sto_corr]]     <- renderUI({testInput(input[[X2_sto_corr]], FullModel_VAR$stoCorr, FALSE, TRUE)}),
     output[[error_NI]]              <- renderUI({testInput(input[[NI]], FullModel_VAR$NI, TRUE, TRUE, (input[[NI]]%%input[[NG]] != 0))}),
     output[[error_Ve]]              <- renderUI({testInput(input[[Ve]], FullModel_VAR$Ve, FALSE, TRUE)}),
     output[[error_NG]]              <- renderUI({testInput(input[[NG]], FullModel_VAR$NG, TRUE, TRUE, (input[[NI]]%%input[[NG]] != 0))}),
     output[[error_VG]]              <- renderUI({testInput(input[[VG]], FullModel_VAR$VG, FALSE, TRUE)}),
     output[[error_NR]] <- renderUI({
          input[[Tmax]];input[[Vhsi]];
          testInput(input[[NR]], FullModel_VAR$NR, TRUE, TRUE) 
       }),
     output[[error_B]]               <- renderUI({testInputBMatrix(input[[B]] , FullModel_VAR$B, TRUE)}),
     output[[error_Vind]]            <- renderUI({testInputVindMatrix(input[[Vind]], FullModel_VAR$Vind, TRUE)}),

    ######################################################################################
    ################################## DOWNLOAD DATA #####################################
    ######################################################################################

    output[[download_sampled]] <- downloadHandler(
      filename = function() {
        paste0('sampled_data_', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(myFullModel()$sampled_data, con)
      }
    ),

    output[[download_raw]] <- downloadHandler(
      filename = function() {
        paste0('raw_data_', Sys.Date(), '.csv')
      },
      content = function(con) {
        write.csv(myFullModel()$full_data, con)
      }
    ),

    output[[Data_Description_Table]] <- renderUI({

      myTable <- data.frame(
        "Output data variable"= c("Output data variable",
                                  "Replicate",
                                  "Individual",
                                  "Individual_Trait",
                                  "Trait",
                                  "Time",
                                  "Phenotype",
                                  "Beta0",
                                  "Beta1",
                                  "Beta2",
                                  "Beta12",
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
#       inFile <- input[[load_inputs]]
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
#     output[[save_inputs]] <- downloadHandler(
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