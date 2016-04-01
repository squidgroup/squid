#Server functions for module 1 step 3
c(
  
    ######### Set variables ######### 
    Mod1Step3updateB <- function(input){
      B <- sqrt(1-input$Mod1Step3_Vi-input$Mod1Step3_Ve)
      return(ifelse(is.nan(B),0,B))
    },
      # Set hidden variables (Tmax, Vi, X1_state, X1_sto_V and NR)
       output$Mod1Step3_hidden <- renderUI({
          list(
            numericInput("Mod1Step3_Tmax", "", Modules_VAR$Tmax$max),
            matrixInput2("Mod1Step3_Vind", "",data.frame(matrix(c(input$Mod1Step3_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS))),
            numericInput("Mod1Step3_Vbx","", 1-input$Mod1Step3_Vi-input$Mod1Step3_Ve),
            matrixInput2("Mod1Step3_B", "",data.frame(matrix(c(0,Mod1Step3updateB(input),0,0),1))),
            checkboxInput("Mod1Step3_X1_state", "", value = TRUE),
            checkboxInput("Mod1Step3_X1_sto_state", "", value = TRUE),
            checkboxInput("Mod1Step3_X1_sto_shared", "", value = FALSE),
            numericInput("Mod1Step3_X1_sto_V","", 1, min = 0, max = 1, step = 0.001)
          )
        }),
 	outputOptions(output, "Mod1Step3_hidden", suspendWhenHidden = FALSE),
 	
	  # display variable (Unknown environmental effect Vbx)
	  output$Mod1Step3_Vbx_txt <- renderUI({

	    if(!testInput(input$Mod1Step3_Vbx, Modules_VAR$Vb1x1, FALSE, FALSE)){
	      output <- span(strong(round(input$Mod1Step3_Vbx,2),class="alert alert-danger"))
	    }else{
	      output <- span(round(input$Mod1Step3_Vbx,2))
	    }

	    p(HTML(paste(strong(Modules_VAR$Vb1x1$label), output,"")))
	  }),
      
    ######### Run simulation #########
   	# Run simulation and return results
   	Mod1Step3_output <- reactive({
   	  if(input$Mod1Step3_Run == 0) # if Run button is pressed
   	    return(NULL)
   	  
   	  isolate({  
   	    
   	    updateCheckboxInput(session, "isRunning", value = TRUE)
   	    
   	    # Call app main function
   	    data <- SQUID::squidR(input, module="Mod1Step3") 
   	    
   	    LMR      <- lme4::lmer(Phenotype ~ 1 + (1|Individual), data = data$sampled_Data)
   	    RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   	    
   	    data$Vi            <- round(RANDEF[1],2)
   	    data$Vr            <- round(RANDEF[2],2) 
   	    data$Vp            <- round(data$Vi + data$Vr,2)
   	    data$phenotypeMean <- round(mean(data$sampled_Data$Phenotype),2)
   	    data$R             <- round(data$Vi / (data$Vi + data$Vr),2)
   	    
   	    updateCheckboxInput(session, "isRunning", value = FALSE)
   	    
   	    return(data)
   	  })  
   	}),     
    
    ######### Display results (graph) #########
     	# Graph: density distribution of true values (Vp)
     	#        density distribution of blups (Vi)
     	#        density distribution of environnemental effect (Vx1)
     	#        density distribution of deviation from blups (Ve)
     	output$Mod1Step3_plot <- renderPlot({ 
     	  
     	  data      <- Mod1Step3_output()
     	  
     	  if(!is.null(data)){
     	    
     	    Vp        <- paste("Vp = "   , data$Vp)
     	    Vi        <- paste("Vi = "   , data$Vi)
     	    Vr        <- paste("Vr (Vbx + Vm) = "  , data$Vr)
     	    
     	    myFactor  <- factor(rep(c(Vp,Vi,Vr), each=length(data$sampled_Data$Phenotype)), levels=c(Vp,Vi,Vr))      
     	    
     	    mydata    <- data.frame(dens  = c(data$sampled_Data$Phenotype, 
     	                                      data$sampled_Data$I,
     	                                      (data$sampled_Data$B1 * data$sampled_Data$X1) + data$sampled_Data$e),
     	                            lines = myFactor)
     	    
     	    print(lattice::densityplot(~dens|lines,data=mydata,
     	                      plot.points = T,as.table=TRUE,
     	                      xlab="Model component values",
     	                      ylab="Density"))            
     	    
     	  }else{
     	    print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
     	  }
     	       	  
   	}),
 	
 	  # Scatter plot: measurements correlation
   	output$Mod1Step3_plot2 <- renderPlot({ 
   	  
   	  if(!is.null(Mod1Step3_output())){
   	    
   	    data         <- Mod1Step3_output()$sampled_Data
   	    phen_time1   <- subset(data, data$Time == data$Time[1], select=Phenotype)
   	    phen_time2   <- subset(data, data$Time == data$Time[2], select=Phenotype)
   	    
   	    plot(phen_time2$Phenotype~phen_time1$Phenotype,
   	         xlab="First measurement", 
   	         ylab="Second measurement",
   	         pch = 19,
   	         col = color$color2)
   	  }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
   	}),
 	
   	# Display results (table)
   	output$Mod1Step3_summary_table <- renderUI({ 
   	  
   	  myTable <- data.frame("True"     = c(paste("Total phenotypic variance ($V_",NOT$total,"$) = 1",sep=""),
   	                                       paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod1Step3_Vi),
   	                                       paste("Residual variance ($V_{",EQ3$mean1," ",EQ2$env1,"}+V_",NOT$error,"$) =",input$Mod1Step3_Ve+input$Mod1Step3_Vbx),
   	                                       "Mean of the trait ($\\mu$) = 0"),
   	                        "Estimated" = c(paste("Total Phenotypic variance in sample ($V'_",NOT$total,"$) = ",ifelse(!is.null(Mod1Step3_output()),Mod1Step3_output()$Vp,"...")),
               	                            paste("Sampled Individual variance ($V'_",NOT$devI,"$) = "      ,ifelse(!is.null(Mod1Step3_output()),Mod1Step3_output()$Vi,"...")),
               	                            paste("Residual variance of sample ($V'_",NOT$residual,"$) = "        ,ifelse(!is.null(Mod1Step3_output()),Mod1Step3_output()$Vr,"...")),
               	                            paste("Sampled mean of the trait ($\\mu'$) = "        ,ifelse(!is.null(Mod1Step3_output()),Mod1Step3_output()$phenotypeMean,"...")))
               	)
   	  
   	  getTable(myTable)
   	}),
 	
   	# display results: repeatability (text)
   	output$Mod1Step3_Rep_txt   <- renderText({ HTML(paste("Your repeatability is $",NOT$repeatability,"$ =", ifelse(!is.null(Mod1Step3_output()), 
 	                                                                                          Mod1Step3_output()$R,"...")))}),
    ######### Manage errors #########
     	# display error message
     	observe({
     	  if(!testInput(input$Mod1Step3_Vbx, Modules_VAR$Vb1x1, FALSE, FALSE)){
     	    updateButton(session, "Mod1Step3_Run", disabled = TRUE, style = Modules_VAR$Run$invalidStyle)
     	  }else{
     	    updateButton(session, "Mod1Step3_Run", disabled = FALSE, style = Modules_VAR$Run$style)
     	  }
     	}), 
 	    output$Mod1Step3_error_Vbx  <- renderUI({testInput(input$Mod1Step3_Vbx, Modules_VAR$Vb1x1, FALSE, TRUE)})
 	            
  ) # End return