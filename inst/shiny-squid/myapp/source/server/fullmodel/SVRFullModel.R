#Server functions for the full model
SVRFullModel <- function(myModule, input, output, session){
  
  myEquations        <- paste(myModule, "myEquations", sep="_")
  
  Tmax               <- paste(myModule, "Tmax", sep="_")
  NT                 <- paste(myModule, "NT", sep="_")
  NP                 <- paste(myModule, "NP", sep="_")
  NI                 <- paste(myModule, "NI", sep="_")
  Vme                <- paste(myModule, "Vme", sep="_")
  NK                 <- paste(myModule, "NK", sep="_")
  Vk                 <- paste(myModule, "Vk", sep="_")
  NR                 <- paste(myModule, "NR", sep="_")
  Vit                <- paste(myModule, "Vit", sep="_")

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
  X1_ran_state       <- paste(myModule, "X1_ran_state", sep="_")
  X1_ran_shared      <- paste(myModule, "X1_ran_shared", sep="_")
  X1_shared          <- paste(myModule, "X1_shared", sep="_")
  X1_ran_V           <- paste(myModule, "X1_ran_V", sep="_")
  X1_ran_corr        <- paste(myModule, "X1_ran_corr", sep="_")
  X1_lin_state       <- paste(myModule, "X1_lin_state", sep="_")
  X1_lin_shared      <- paste(myModule, "X1_lin_shared", sep="_")
  X1_cyc_state       <- paste(myModule, "X1_cyc_state", sep="_")
  X1_cyc_shared      <- paste(myModule, "X1_cyc_shared", sep="_")
  X1_plotEnvironment <- paste(myModule, "X1_plotEnvironment", sep="_")
  
  X2_state           <- paste(myModule, "X2_state", sep="_")
  X2_ran_state       <- paste(myModule, "X2_ran_state", sep="_")
  X2_ran_shared      <- paste(myModule, "X2_ran_shared", sep="_")
  X2_ran_V           <- paste(myModule, "X2_ran_V", sep="_")
  X2_ran_corr        <- paste(myModule, "X2_ran_corr", sep="_")
  X2_lin_state       <- paste(myModule, "X2_lin_state", sep="_")
  X2_lin_shared      <- paste(myModule, "X2_lin_shared", sep="_")
  X2_cyc_state       <- paste(myModule, "X2_cyc_state", sep="_")
  X2_cyc_shared      <- paste(myModule, "X2_cyc_shared", sep="_")
  X2_shared          <- paste(myModule, "X2_shared", sep="_")
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
  
  Dtime_Ind          <- paste(myModule, "Dtime_Ind", sep="_")
  Dtime_Trait        <- paste(myModule, "Dtime_Trait", sep="_")
  Drec_Ind           <- paste(myModule, "Drec_Ind", sep="_")
  Drec_Trait         <- paste(myModule, "Drec_Trait", sep="_")
  
  download_sampled      <- paste(myModule, "download_sampled", sep="_")
  download_raw          <- paste(myModule, "download_raw", sep="_")
  save_inputs           <- paste(myModule, "save_inputs", sep="_")
  load_inputs           <- paste(myModule, "load_inputs", sep="_")
  
  error_Tmax            <- paste(myModule, "error_Tmax", sep="_")
  error_NP              <- paste(myModule, "error_NP", sep="_")    
  error_X1_ran_V        <- paste(myModule, "error_X1_ran_V", sep="_")
  error_X2_ran_V        <- paste(myModule, "error_X2_ran_V", sep="_")
  error_X1_ran_corr     <- paste(myModule, "error_X1_ran_corr", sep="_")
  error_X2_ran_corr     <- paste(myModule, "error_X2_ran_corr", sep="_")
  error_NI              <- paste(myModule, "error_NI", sep="_")
  error_NK              <- paste(myModule, "error_NK", sep="_")
  error_Vme             <- paste(myModule, "error_Vme", sep="_")
  error_Vk              <- paste(myModule, "error_Vk", sep="_")
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
         data <- SQUID::runSQUIDfct(input, myModule, TRUE) 
         
         names(data$full_Data)    <- outputNames
         names(data$sampled_Data) <- outputNames
         
         updateCheckboxInput(session, "isRunning", value = FALSE)
         
         return(data)
         
       })              
     }),

     output[[plotEnvironment]] <- renderPlot({
          
       data <- myFullModel()
       #   print result graphs 
       print(SQUID::multiplot(data$myPlot$plotX1,                    
                              data$myPlot$plotX2,
                              data$myPlot$plotX1X2,
                              cols=1))
       
     }),
     
     output[[plotPhenotype]] <- renderPlot({
         
       data <- myFullModel() 
       #   print result graphs 
       print(SQUID::multiplot(data$myPlot$plotTotPhen,                    
                              data$myPlot$plotSampPhen,                    
                              cols=1))
       
     }),
     
     output[[plotSamples]] <- renderPlot({
       data <- myFullModel()
       #   print result graphs 
       print(data$myPlot$plotSampTime)
     }),
     
     # update the sampling time length for each individual    
     output[[SampTime]] <- renderText({ 
       FullModel_VAR$NR$max <<- round(input[[Tmax]]*(1-input[[Vit]])) 
     }),

     # Update sampling time checkbox relative to the sampling record checkbox
     observe({
       if(input[[Dtime_Ind]])   updateCheckboxInput(session, Drec_Ind,   value = TRUE)
       if(input[[Dtime_Trait]]) updateCheckboxInput(session, Drec_Trait, value = TRUE)
       
       if(!input[[Drec_Ind]] & input[[Dtime_Ind]])       updateCheckboxInput(session, Drec_Ind,   value = TRUE)
       if(!input[[Drec_Trait]] & input[[Dtime_Trait]])   updateCheckboxInput(session, Drec_Trait, value = TRUE)  
       
       if(input[[Vit]] > 0){
         updateCheckboxInput(session, Dtime_Ind,   value = FALSE)
         updateCheckboxInput(session, Dtime_Trait,   value = FALSE)
       }
     }),

     ######################################################################################
     ################################## ENVIRONMENT #######################################
     ######################################################################################

      # Update environment states
      observe({
         ifelse(input[[X1_ran_state]] || input[[X1_lin_state]]|| input[[X1_cyc_state]], 
                updateCheckboxInput(session, X1_state, value = TRUE), 
                updateCheckboxInput(session, X1_state, value = FALSE))
        
        # Update X2 environment state
        ifelse(input[[X2_ran_state]] || input[[X2_lin_state]]|| input[[X2_cyc_state]], 
               updateCheckboxInput(session, X2_state, value = TRUE), 
               updateCheckboxInput(session, X2_state, value = FALSE))
        
        if(!input[[X1_ran_state]]) updateCheckboxInput(session, X1_ran_shared, value = TRUE)
        if(!input[[X1_lin_state]]) updateCheckboxInput(session, X1_lin_shared, value = TRUE)
        if(!input[[X1_cyc_state]]) updateCheckboxInput(session, X1_cyc_shared, value = TRUE)
        
        if(!input[[X2_ran_state]]) updateCheckboxInput(session, X2_ran_shared, value = TRUE)  
        if(!input[[X2_lin_state]]) updateCheckboxInput(session, X2_lin_shared, value = TRUE)
        if(!input[[X2_cyc_state]]) updateCheckboxInput(session, X2_cyc_shared, value = TRUE)
        
        # Interaction state
        if(!input[[X1_state]] || !input[[X2_state]]) updateCheckboxInput(session, X_Interaction, value = FALSE)
      }),

      output[[X1_plotEnvironment]] <- renderPlot({SQUID::showEnvironment(input, myModule, "X1")}),
      output[[X2_plotEnvironment]] <- renderPlot({SQUID::showEnvironment(input, myModule, "X2")}),
     
    ######################################################################################
    ############################### VARIANCES SUMMARY ####################################
    ######################################################################################

    output[[variancesTable]] <- renderUI({  
    
      VarianceNames <- c("$\\text{Fixed effects}$",
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
                         paste("$V_",NOT$error,"$",sep=""),
                         paste("$V_",NOT$total,"$",sep="")
                         )
      
      Explanation  <- c(" ",
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
                        "Measurement error variance", 
                        "Total phenotypic variance"
                      )
      
      myTable <- data.frame(
                  "Variance"    = VarianceNames,
                  "Explanation" = Explanation
                )
      
      mySummary <- SVRGetSummaryVariances(input,B,Vind,Vme,Vk,NT,0,nb.IS,NOT$trait.1,X1_state,X2_state,X_Interaction)
      myTable   <- cbind(myTable,mySummary)
      
      if(input[[NT]] > 1){
        mySummary <- SVRGetSummaryVariances(input,B,Vind,Vme,Vk,NT,nb.IS,nb.IS,NOT$trait.2,X1_state,X2_state,X_Interaction)
        myTable   <- cbind(myTable,mySummary)
        myTable   <- subset(myTable, Trait.y != "0 (0%)" | Trait.z != "0 (0%)")
      }else{
        myTable <- subset(myTable, Trait.y != "0 (0%)")
      }

      getTable(myTable)
      
    }),

    ######################################################################################
    ################################## ERROR MANAGER #####################################
    ######################################################################################

     isError <- reactive({
       
      FullModel_VAR$NR$max    <<- round(input[[Tmax]]*(1-input[[Vit]])) 
      FullModel_VAR$NK$modulo <<- input[[NI]] 
      
      if(!testInput(input[[Tmax]], FullModel_VAR$Tmax, TRUE, FALSE)                  ||
         !testInput(input[[NP]], FullModel_VAR$NP, TRUE, FALSE)                      ||            
         !testInput(input[[X1_ran_V]], FullModel_VAR$ranV, FALSE, FALSE)             ||
         !testInput(input[[X2_ran_V]], FullModel_VAR$ranV, FALSE, FALSE)             ||
         !testInput(input[[X1_ran_corr]], FullModel_VAR$ranCorr, FALSE, FALSE)       || 
         !testInput(input[[X2_ran_corr]], FullModel_VAR$ranCorr, FALSE, FALSE)       || 
         !testInput(input[[NI]], FullModel_VAR$NI, TRUE, FALSE)                      ||
         !testInput(input[[Vme]], FullModel_VAR$Vme, FALSE, FALSE)                   ||
         !testInput(input[[Vk]], FullModel_VAR$Vk, FALSE, FALSE)                     ||
         !testInput(input[[NK]], FullModel_VAR$NK, TRUE, FALSE, TRUE)                ||
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
         updateButton(session, runButton, disabled = TRUE, style = FullModel_VAR$Run$invalidStyle)
         updateButton(session, rerunButton, disabled = TRUE, style = FullModel_VAR$ReRun$invalidStyle)     
       }else{
         updateButton(session, runButton, disabled = FALSE, style = FullModel_VAR$Run$style)
         updateButton(session, rerunButton, disabled = FALSE, style = FullModel_VAR$ReRun$style)
       }
     }),
    
     output[[error_Tmax]]            <- renderUI({testInput(input[[Tmax]], FullModel_VAR$Tmax, TRUE, TRUE)}),
     output[[error_NP]]              <- renderUI({testInput(input[[NP]], FullModel_VAR$NP, TRUE, TRUE)}),     
     output[[error_X1_ran_V]]        <- renderUI({testInput(input[[X1_ran_V]], FullModel_VAR$ranV, FALSE, TRUE)}),
     output[[error_X2_ran_V]]        <- renderUI({testInput(input[[X2_ran_V]], FullModel_VAR$ranV, FALSE, TRUE)}),
     output[[error_X1_ran_corr]]    <- renderUI({testInput(input[[X1_ran_corr]], FullModel_VAR$ranCorr, FALSE, TRUE)}),
     output[[error_X2_ran_corr]]    <- renderUI({testInput(input[[X2_ran_corr]], FullModel_VAR$ranCorr, FALSE, TRUE)}),
     output[[error_NI]]              <- renderUI({testInput(input[[NI]], FullModel_VAR$NI, TRUE, TRUE)}),
     output[[error_Vme]]             <- renderUI({testInput(input[[Vme]], FullModel_VAR$Vme, FALSE, TRUE)}),
     output[[error_NK]]              <- renderUI({testInput(input[[NK]], FullModel_VAR$NK, TRUE, TRUE, TRUE)}),
     output[[error_Vk]]              <- renderUI({testInput(input[[Vk]], FullModel_VAR$Vk, FALSE, TRUE)}),
     output[[error_NR]] <- renderUI({
          input[[Tmax]];input[[Vit]];
          testInput(input[[NR]], FullModel_VAR$NR, TRUE, TRUE) 
       }),
     output[[error_B]]               <- renderUI({testInputBMatrix(input[[B]] , FullModel_VAR$B, TRUE)}),
     output[[error_Vind]]            <- renderUI({testInputVindMatrix(input[[Vind]], FullModel_VAR$Vind, TRUE)}),

    ######################################################################################
    ################################## DOWNLOAD DATA #####################################
    ######################################################################################

    output[[download_sampled]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep="")
      },
      content = function(con) {
        write.csv(myFullModel()$sampled_Data, con)
      }
    ),

    output[[download_raw]] <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep="")
      },
      content = function(con) {
        write.csv(myFullModel()$full_Data, con)
      }
    ),

    output[[Data_Description_Table]] <- renderUI({

      myTable <- data.frame(
        "Output data variable"= c("Replicate",
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
                              "X12",
                              "G",
                              "e"),
        "Mathematical symbol" = c("",
                    paste0("$",NOT$ind,"$"),
                    "",
                    paste0("$",NOT$trait.1,"$ (1) and $",NOT$trait.2,"$ (2)"),
                    paste0("$",NOT$time,"$"),
                    paste0("$",EQ$phen.1,"$ and $",EQ$phen.2,"$"),
                    paste0("$",EQ3$mean0,"$"),
                    paste0("$",EQ3$mean1,"$"),
                    paste0("$",EQ3$mean2,"$"),
                    paste0("$",EQ3$mean12,"$"),
                    paste0("$",NOT$devI,"$"),
                    paste0("$",EQ3$dev1,"$"),
                    paste0("$",EQ3$dev2,"$"),
                    paste0("$",EQ3$dev12,"$"),
                    paste0("$",EQ2$env1,"$"),
                    paste0("$",EQ2$env2,"$"),
                    paste0("$",EQ2$env12,"$"),
                    paste0("$",NOT$groupV,"$"),
                    paste0("$",NOT$error,"$")),
        "Description" = c("Replicate identification",
                    "Individual identification",
                    "Individual-trait identification",
                    "Trait identification",
                    "Time step values",
                    "Individual phenotype",
                    "Population phenotypic mean",
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
                    paste0("Measurement error."))
      )  
      
      return(getTable(myTable))
      
    })#,
  
#     output[[save_inputs]] <- downloadHandler(
#       filename = function() {
#         paste('inputs-', Sys.Date(), '.RData', sep="")
#       },
#       content = function(con) {
#         saveFile <- reactiveValuesToList(input)
#         save(saveFile, file = con)
#       }
#     ),

#     observe({
#       
#       inFile <- input[[load_inputs]]
#       if (is.null(inFile))
#         return(NULL)
# 
#       load(inFile$datapath)
#       isolate(
#         updateNumericInput(session, inputId = NI, value = saveFile[[NI]])
#       )
#       
#     })

  )) # End return
}