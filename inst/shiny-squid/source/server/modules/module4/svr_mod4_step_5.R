#Server functions for module 4 step 5
c(
    ######### Set variables #########    
    # Set hidden variables
    Mod4Step5updateVind <- function(input, nb.IS){
      m <- matrix(rep(0,(nb.IS*2)^2),(nb.IS*2))
      diag(m)[1]         <- input$Mod4Step5_Vi1
      diag(m)[1 + nb.IS] <- input$Mod4Step5_Vi2
      m[nb.IS + 1,1]     <- input$Mod4Step5_Corr_I
      return(m)
    },
    Mod4Step5updateB <- function(input, nb.IS){
    	m <- matrix(rep(0,(nb.IS*2)), nrow = 1)
    	m[1,2]             <- input$Mod4Step5_B11
    	m[1,nb.IS + 2]     <- input$Mod4Step5_B12
    	return(m)
    },
     output$Mod4Step5_hidden <- renderUI({
        list(
          numericInput("Mod4Step5_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step5_NI", "", 100),
          numericInput("Mod4Step5_NT", "", 2),
          numericInput("Mod4Step5_NR", "", 10),
          shinyMatrix::matrixInput("Mod4Step5_B", value = Mod4Step5updateB(input, nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod4Step5_Vind", value = Mod4Step5updateVind(input, nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod4Step5_Ve", value = matrix(c(input$Mod4Step5_Ve1,   input$Mod4Step5_Corr_e,
                                                       input$Mod4Step5_Corr_e, input$Mod4Step5_Ve2), 2), class = "numeric"),
          
          checkboxInput("Mod4Step5_X1_state", "",     value = TRUE),
          checkboxInput("Mod4Step5_X1_sto_state", "", value = TRUE)
        )
     }),
    outputOptions(output, "Mod4Step5_hidden", suspendWhenHidden = FALSE),

    # get data from squidR()
    get_data <- function(){
      
      # Call app main function
      data <- squid::squidR(input, module = "Mod4Step5")
      
      dt <- as.data.table(data$sampled_data)
      dt <- dt[ , .(Time, Individual, Trait, Phenotype, X1)]
      dt[ , Trait := paste0("Phenotype_", Trait)]
      dt <- dcast(dt, Time + Individual + X1 ~ Trait, value.var = "Phenotype")
      
      return(list("sampled" = dt, "full_data" = data$full_data))
    },
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod4Step5_output <- reactive({
    	if (input$Mod4Step5_Run == 0) # if Run button is pressed
    		return(NULL)
    	
    	isolate({
    		
    		updateCheckboxInput(session, "isRunning", value = TRUE)
    	  dt <- get_data()
    		updateCheckboxInput(session, "isRunning", value = FALSE)
    		
    		return(dt)
    	})
    }),
    
    observe({
      
      data  <- Mod4Step5_output()
      
      if(is.null(data)){
        disableActionButton("Mod4Step5_Run_1", session, "true")
        disableActionButton("Mod4Step5_Run_2", session, "true")
      }else{
        disableActionButton("Mod4Step5_Run_1", session, "false")
        disableActionButton("Mod4Step5_Run_2", session, "false")
      }
    }),
    
    Mod4Step5_output_model1 <- reactive({
      
      if (input$Mod4Step5_Run_1 == 0) # if Run button is pressed
        return(NULL)
      
      isolate({
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        data  <- get_data()$sampled
        
        library(brms)
        fit1 <- readRDS("./source/server/modules/module4/stanFiles/module4_step5_brms_model1.rds")
        fit1 <- update(fit1, newdata = data,
                       iter = 200, warmup = 100, chains = 1)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
       
        return(fit1)
        
      })
    }),
    
    Mod4Step5_output_model2 <- reactive({
      
      if (input$Mod4Step5_Run_2 == 0) # if Run button is pressed
        return(NULL)
      
      isolate({
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        data  <- get_data()$sampled
        
        library(brms)
        fit2 <- readRDS("./source/server/modules/module4/stanFiles/module4_step5_brms_model2.rds")
        fit2 <- update(fit2, newdata = data,
                       iter = 200, warmup = 100, chains = 1)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(fit2)
        
      })
    }),
    
    output$Mod4Step5_correlationplot <- renderPlot({
    	
    	data  <- Mod4Step5_output()
    	
    	if (!is.null(data)) {
    		
    		dt <- copy(data[["sampled"]])
    		
    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Phenotype of trait y") +
    			ggplot2::ylab("Phenotype of trait z") + 
    		  ggplotCustomTheme()
    		
    	}else{
    	  sim_msg()
    		# defaultPlot()
    	}
    	
    }),
    
    output$Mod4Step5_correlationplot2 <- renderPlot({

    	data  <- Mod4Step5_output()

    	if (!is.null(data)) {

    		dt <- copy(data[["sampled"]])
    		dt <- dt[ , .(Phenotype_1 = mean(Phenotype_1),
    									Phenotype_2 = mean(Phenotype_2)), by = Individual]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
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
    		dt[ , ':='(Phenotype_1 = Phenotype_1 - mean(Phenotype_1),
    							 Phenotype_2 = Phenotype_2 - mean(Phenotype_2)), by = Individual]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
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
    
    
    Matrices <- reactive({
      
      cov1 <- round(input$Mod4Step5_Corr_I * sqrt(input$Mod4Step5_Vi1 * input$Mod4Step5_Vi2),3)
      eq1 <- paste0(
        "$$ \\Omega_{",NOT$devI,"}=
        		\\begin{pmatrix}
          		V_{",NOT$devI,"_",NOT$trait.1,"} & Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}  \\\\
          		Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} & V_{", NOT$devI,"_",NOT$trait.2,"} \\\\
        		\\end{pmatrix} 
        		=
        		\\begin{pmatrix}
          		",input$Mod4Step5_Vi1," & ",cov1," \\\\
          		",cov1," & ",input$Mod4Step5_Vi2,"\\\\
        		\\end{pmatrix} 
    		 $$")
      
      cov2 <- round(input$Mod4Step5_Corr_e * sqrt(input$Mod4Step5_Ve1 * input$Mod4Step5_Ve2),3)
      eq2 <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
        		\\begin{pmatrix}
          		V_{",NOT$error,"_",NOT$trait.1,"} & Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}  \\\\
          		Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"} & V_{", NOT$error,"_",NOT$trait.2,"} \\\\
        		\\end{pmatrix} 
        		=
        		\\begin{pmatrix}
          		",input$Mod4Step5_Ve1," & ",cov2," \\\\
          		",cov2," & ",input$Mod4Step5_Ve2,"\\\\
        		\\end{pmatrix} 
    		 $$")
      
      return(withMathJax(paste0(eq1, eq2)))
      
    }),
    
    output$Mod4Step5_Matrices_1 <- renderUI({Matrices()}),
    output$Mod4Step5_Matrices_2 <- renderUI({Matrices()}),
    output$Mod4Step5_Matrices_3 <- renderUI({Matrices()}),
    
    
    show_model_variance_results <- function(fit){
      
      RanCoef <- VarCorr(fit)
      varI1   <- round(RanCoef[["Individual"]][["sd"]]["Phenotype1_Intercept", "Estimate"]^2, 2)
      varI2   <- round(RanCoef[["Individual"]][["sd"]]["Phenotype2_Intercept", "Estimate"]^2, 2)
      covI12  <- round(RanCoef[["Individual"]][["cov"]]["Phenotype1_Intercept", "Estimate", "Phenotype2_Intercept"], 2)
      
      varE1   <- round(RanCoef[["residual__"]][["sd"]]["Phenotype1", "Estimate"]^2, 2)
      varE2   <- round(RanCoef[["residual__"]][["sd"]]["Phenotype2", "Estimate"]^2, 2)
      covE12  <- round(RanCoef[["residual__"]][["cov"]]["Phenotype1", "Estimate", "Phenotype2"], 2)
      
      
      
      cov1 <- round(input$Mod4Step5_Corr_I * sqrt(input$Mod4Step5_Vi1 * input$Mod4Step5_Vi2),3)
      cov2 <- round(input$Mod4Step5_Corr_e * sqrt(input$Mod4Step5_Ve1 * input$Mod4Step5_Ve2),3)
      
      eq1 <- paste0(
        "$$\\Omega_{",NOT$devI,"'}=
        		\\begin{pmatrix}
        		",varI1,"\\,[",input$Mod4Step5_Vi1,"] & ",covI12,"\\,[",cov1,"] \\\\
        		",covI12,"\\,[",cov1,"] & ",varI2,"\\,[",input$Mod4Step5_Vi2,"]\\\\
        		\\end{pmatrix}\\quad")
      
      eq2 <- paste0(
        "\\Omega_{",NOT$error,"'}=
        		\\begin{pmatrix}
        		",varE1,"\\,[",input$Mod4Step5_Ve1,"] & ",covE12,"\\,[",cov2,"] \\\\
        		",covE12,"\\,[",cov2,"] & ",varE2,"\\,[",input$Mod4Step5_Ve1,"]\\\\
        		\\end{pmatrix} 
        		$$")
      
      return(paste0(eq1, eq2))
      
    },
    
    
    show_model_correlation_results <- function(fit){
      
      RanCoef <- VarCorr(fit)
      
      corI12 <- round(RanCoef[["Individual"]][["cor"]]["Phenotype1_Intercept", "Estimate", "Phenotype2_Intercept"],2)
      corE12 <- round(RanCoef[["residual__"]][["cor"]]["Phenotype1", "Estimate", "Phenotype2"],2)
      
      cor <- paste0("$$\\text{Among-individual correlation: }",corI12,"\\,[",input$Mod4Step5_Corr_I,"]$$
                     $$\\text{Residual within-individual correlation: }",corE12,"\\,[",input$Mod4Step5_Corr_e,"]$$")
      
      return(cor)
      
    },
    
    output$Mod4Step5_Result_Matrices_Model1 <- renderUI({
      
      fit1  <- Mod4Step5_output_model1()
      
      if (!is.null(fit1)) {
        
        isolate({res <- show_model_variance_results(fit1)})
        
        return(withMathJax(res))
        
      }else{return(withMathJax("$$...$$"))}
    }),
    
    output$Mod4Step5_Result_Matrices_Model1_corr <- renderUI({
      
      fit1  <- Mod4Step5_output_model1()
      
      if (!is.null(fit1)) {
        
        isolate({cor <- show_model_correlation_results(fit1)})
        
        return(withMathJax(cor))       
        
      }else{return(withMathJax("$$...$$"))}
      
    }),
    
    output$Mod4Step5_Result_Matrices_Model2 <- renderUI({
      
      fit2  <- Mod4Step5_output_model2()
      
      if (!is.null(fit2)) {
        
        isolate({res <- show_model_variance_results(fit2)})
        
        return(withMathJax(res))
      
      }else{return(withMathJax("$$...$$"))}
    }),
    
    output$Mod4Step5_Result_Matrices_Model2_corr <- renderUI({
      
      fit2  <- Mod4Step5_output_model2()
      
      if (!is.null(fit2)) {
        
        isolate({cor <- show_model_correlation_results(fit2)})
        
        return(withMathJax(cor))       
 
      }else{return(withMathJax("$$...$$"))}
      
    })
    
) # End return
