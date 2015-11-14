#Server functions for module 3 step 1
SVRMod3Step1 <- function(input, output, session, Modules_VAR, nb.IS, color){
  
  return(c(
  
    ######### Set variables #########    
      # Set hidden variables (Tmax, Vi, ES_state, ES_ran_V and NR)
       output$Mod3Step1_hidden <- renderUI({
          list(
            numericInput("Mod3Step1_Tmax", "", Modules_VAR$Tmax$max),
            numericInput("Mod3Step1_NI", "", 5),
            getNumericInput("Mod3Step1_Tmax", Modules_VAR$Tmax, ""),
            matrixInput2("Mod3Step1_Vind", "",data.frame(matrix(c(input$Mod3Step1_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS))),
            numericInput("Mod3Step1_Vx","", 1-input$Mod3Step1_Vi-input$Mod3Step1_Vme),
            matrixInput2("Mod3Step1_B", "",data.frame(matrix(c(0,sqrt(1-input$Mod3Step1_Vi-input$Mod3Step1_Vme),0,0),1))),
            checkboxInput("Mod3Step1_X1_state", "", value = TRUE),
            checkboxInput("Mod3Step1_X1_ran_state", "", value = TRUE),
            checkboxInput("Mod3Step1_X1_ran_shared", "", value = TRUE),
            numericInput("Mod3Step1_X1_ran_V","", 1, min = 0, max = 1, step = 0.001),
            checkboxInput("Mod3Step1_Dtime_Ind", "", value = FALSE)
          )
        }),
 	outputOptions(output, "Mod3Step1_hidden", suspendWhenHidden = FALSE),
 	
 	
 	# display variable (general and unknown environmntal effect)
 	output$Mod3Step1_Vx_txt <- renderUI({
 	  
 	  if(!testInput(input$Mod3Step1_Vx, Modules_VAR$Vegu, FALSE, FALSE)){
 	    output <- span(strong(round(input$Mod3Step1_Vx,2),class="alert alert-danger"))
 	  }else{
 	    output <- span(round(input$Mod3Step1_Vx,2))
 	  }
 	  
 	  p(HTML(paste(strong(Modules_VAR$Vegu$label), output,"")))
 	}),
      
    ######### Run simulation #########
   	# Run simulation and return results
   	Mod3Step1_output <- reactive({
   	  if(input$Mod3Step1_Run == 0) # if Run button is pressed
   	    return(NULL)
   	  
   	  isolate({ 
   	    
   	    updateCheckboxInput(session, "isRunning", value = TRUE)
   	    
   	    # Call app main function
   	    data <- main(input, "Mod3Step1", session, TRUE)  
   	    
   	    LMR      <- lmer(Phenotype ~ 1 + (1|Individual), data = data$data_S)
   	    RANDEF   <- as.data.frame(VarCorr(LMR))$vcov
   	    
   	    data$Vi        <- round(RANDEF[1],2)
   	    data$Vr        <- round(RANDEF[2],2) 
   	    data$Vp        <- round(data$Vi + data$Vr,2)
   	    data$mean      <- round(mean(data$data_S$Phenotype),2)
   	    data$R         <- round(data$Vi / (data$Vi + data$Vr),2)
   	    
   	    updateCheckboxInput(session, "isRunning", value = FALSE)
   	    
   	    return(data)
   	  })  
   	}),  
 	
 	output$Mod3Step1_previewPlot <- renderPlot({ 
 	  
 	  input$Mod3Step1_previewPlot
 	  
 	  myInput <- list("Mod3Step1_Preview_Tmax" = Modules_VAR$Tmax$max,
 	                  "Mod3Step1_Preview_NI" = input$Mod3Step1_NI,
 	                  "Mod3Step1_Preview_Vit" = input$Mod3Step1_Vit,
 	                  "Mod3Step1_Preview_NR" = input$Mod3Step1_NR,
 	                  "Mod3Step1_Preview_Dtime_Ind" = FALSE
 	                  )
 	  # Call app main function
 	  data <- main(myInput, "Mod3Step1_Preview", session, TRUE)
 	  print(data$myPlot$plotSampTime)
 	}),
 	
   	# Display results (table)
   	output$Mod3Step1_summary_table <- renderUI({ 
   	  
   	  data <- Mod3Step1_output()
   	  
   	  myTable <- data.frame("True"       = c(paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod3Step1_Vi),
   	                                         paste("Residual variance ($V_",NOT$error,"$) =",input$Mod3Step1_Vme),
   	                                         paste("Environmental variance ($V_",NOT$env,"$) =",input$Mod3Step1_Vx)),
   	                        "Estimated" = c(paste("Individual variance in sample ($V'_",NOT$devI,"$) = "      ,ifelse(!is.null(data),data$Vi,"...")),
   	                                        paste("Residual variance of sample ($V'_",NOT$residual,"$) = "        ,ifelse(!is.null(data),data$Vr,"...")),
   	                                        "") 
   	  )  
   	  
   	  getTable(myTable) 
   	  
   	}),
 	

    
    ######### Manage errors #########
     	# display error message
     	observe({
     	  if(
#      	     !testInput(input$Mod3Step1_Vi, Modules_VAR$Vi, FALSE, FALSE) ||
#      	     !testInput(input$Mod3Step1_Vme, Modules_VAR$Vme, FALSE, FALSE) ||
     	     !testInput(input$Mod3Step1_Vx, Modules_VAR$Vegu, FALSE, FALSE)){
     	    updateButton(session, "Mod3Step1_Run", disabled = TRUE, style = Modules_VAR$Run$invalidStyle)
     	  }else{
     	    updateButton(session, "Mod3Step1_Run", disabled = FALSE, style = Modules_VAR$Run$style)
     	  }
     	}), 

#  	    output$Mod3Step1_error_Vi  <- renderUI({testInput(input$Mod3Step1_Vi, Modules_VAR$Vi, FALSE, TRUE)}),
#  	    output$Mod3Step1_error_Vme  <- renderUI({testInput(input$Mod3Step1_Vme, Modules_VAR$Vme, FALSE, TRUE)}),
 	    output$Mod3Step1_error_Vx  <- renderUI({testInput(input$Mod3Step1_Vx, Modules_VAR$Vegu, FALSE, TRUE)})
 	            
  )) # End return
}