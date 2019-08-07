#Server functions for module 4 step 5
c(
    ######### Set variables #########    
    # Set hidden variables
    Mod4Step5updateVind <- function(input, nb.IS){
      df <- matrix(rep(0,(nb.IS*2)^2),(nb.IS*2))
      diag(df)[1]         <- input$Mod4Step5_Vi1
      diag(df)[1 + nb.IS] <- input$Mod4Step5_Vi2
      df[nb.IS + 1,1]     <- input$Mod4Step5_Corr_I
      return(as.data.frame(df))
    },
    Mod4Step5updateB <- function(input, nb.IS){
    	df <- matrix(rep(0,(nb.IS*2)), nrow = 1)
    	df[1,2]             <- input$Mod4Step5_B11
    	df[1,nb.IS + 2]     <- input$Mod4Step5_B12
    	return(as.data.frame(df))
    },
     output$Mod4Step5_hidden <- renderUI({
        list(
          numericInput("Mod4Step5_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step5_NI", "", 100),
          numericInput("Mod4Step5_NT", "", 2),
          numericInput("Mod4Step5_NR", "", 10),
          matrixInput2("Mod4Step5_B", "",    Mod4Step5updateB(input, nb.IS)),
          matrixInput2("Mod4Step5_Vind", "", Mod4Step5updateVind(input, nb.IS)),
          matrixInput2("Mod4Step5_Ve", "", data.frame(matrix(c(input$Mod4Step5_Ve1,   input$Mod4Step5_Corr_e,
                                                               input$Mod4Step5_Corr_e, input$Mod4Step5_Ve2), 2))),
          
          checkboxInput("Mod4Step5_X1_state", "",     value = TRUE),
          checkboxInput("Mod4Step5_X1_sto_state", "", value = TRUE)
        )
     }),
    outputOptions(output, "Mod4Step5_hidden", suspendWhenHidden = FALSE),

    
    ######### Run simulation #########
    # Run simulation and return results
    Mod4Step5_output <- reactive({
    	if (input$Mod4Step5_Run == 0) # if Run button is pressed
    		return(NULL)
    	
    	isolate({
    		
    		updateCheckboxInput(session, "isRunning", value = TRUE)
    		
    		# Call app main function
    		data <- squid::squidR(input, module = "Mod4Step5")
    		
    		dt <- as.data.table(data$sampled_data)
    		dt <- dt[ , .(Time, Individual, Trait, Phenotype, X1)]
    		dt[ , Trait := paste0("Trait_", Trait)]
    		dt <- dcast(dt, Time + Individual ~ Trait, value.var = "Phenotype")
    		
    		updateCheckboxInput(session, "isRunning", value = FALSE)
    		
    		return(list("sampled" = dt, "full_data" = data$full_data))
    	})
    }),
    
    output$Mod4Step5_correlationplot <- renderPlot({
    	
    	data  <- Mod4Step5_output()
    	
    	if (!is.null(data)) {
    		
    		dt <- copy(data[["sampled"]])
    		
    		ggplot2::ggplot(dt, ggplot2::aes(x = Trait_1, Trait_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Phenotype of trait y") +
    			ggplot2::ylab("Phenotype of trait z") + 
    		  ggplotCustomTheme()
    		
    	}else{
    		defaultPlot()
    	}
    	
    }),
    
    output$Mod4Step5_correlationplot2 <- renderPlot({

    	data  <- Mod4Step5_output()

    	if (!is.null(data)) {

    		dt <- copy(data[["sampled"]])
    		dt <- dt[ , .(Trait_1 = mean(Trait_1),
    									Trait_2 = mean(Trait_2)), by = Individual]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Trait_1, Trait_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Mean phenotype of trait y") +
    			ggplot2::ylab("Mean phenotype of trait z") + 
    		  ggplotCustomTheme()

    	}else{
    		defaultPlot()
    	}

    }),

    output$Mod4Step5_correlationplot3 <- renderPlot({

    	data  <- Mod4Step5_output()

    	if (!is.null(data)) {

    		dt <- copy(data[["sampled"]])
    		dt[ , ':='(Trait_1 = Trait_1 - mean(Trait_1),
    							 Trait_2 = Trait_2 - mean(Trait_2)), by = Individual]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Trait_1, Trait_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Deviation from individual phenotype mean of trait y") +
    			ggplot2::ylab("Deviation from individual phenotype mean of trait z") + 
    		  ggplotCustomTheme()

    	}else{
    		defaultPlot()
    	}

    }),

    output$Mod4Step5_environment <- renderPlot({

    	data  <- Mod4Step5_output()

    	if (!is.null(data)) {

    		dt <- copy(data[["full_data"]])
    		dt <-  dt[1:(input$Mod4Step5_Tmax),]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Time, y = X1,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Time") +
    			ggplot2::ylab("Environment value") + 
    		  ggplotCustomTheme()

    	}else{
    		defaultPlot()
    	}

    }),
    
     output$Mod4Step5_Phenotypic_Equation <- renderUI({

     	eq <- paste0("$$",
     					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
     					 (0 + ",EQ$dev0.1,") + 
     					 ",input$Mod4Step5_B11,NOT$env,"_{",NOT$time,NOT$ind,"} +
     					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
     					 
     					 $$", 
     					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
     					 (0 + ",EQ$dev0.2,") + 
     					 ",input$Mod4Step5_B12,NOT$env,"_{",NOT$time,NOT$ind,"} +
     					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
     					 $$")
     	
       return(withMathJax(eq))
     }),
    
    output$Mod4Step5_Result_Matrices <- renderUI({
    	
    	cov1 <- round(input$Mod4Step5_Corr_I * sqrt(input$Mod4Step5_Vi1 * input$Mod4Step5_Vi2),3)
    	eq1 <- paste0(
    		"$$ \\Omega_{",NOT$devI,"}=
    		\\begin{pmatrix}
    		",input$Mod4Step5_Vi1," & ",cov1," \\\\
    		",cov1," & ",input$Mod4Step5_Vi2,"\\\\
    		\\end{pmatrix} 
    		$$")
    	
    	cov2 <- round(input$Mod4Step5_Corr_e * sqrt(input$Mod4Step5_Ve1 * input$Mod4Step5_Ve2),3)
    	eq2 <- paste0(
    		"$$ \\Omega_{",NOT$error,"}=
    		\\begin{pmatrix}
    		",input$Mod4Step5_Ve1," & ",cov2," \\\\
    		",cov2," & ",input$Mod4Step5_Ve2,"\\\\
    		\\end{pmatrix} 
    		$$")
    	
    	return(withMathJax(paste0(eq1, eq2)))
    })
    
) # End return
