#Server functions for module 4 step 2
c(
    ######### Set variables #########    
    # Set hidden variables
    Mod4Step2updateVind <- function(input, nb.IS){
      m <- matrix(rep(0,(nb.IS*2)^2),(nb.IS*2))
      diag(m)[1]         <- input$Mod4Step2_Vi1
      diag(m)[1 + nb.IS] <- input$Mod4Step2_Vi2
      return(m)
    },
     output$Mod4Step2_hidden <- renderUI({
        list(
          numericInput("Mod4Step2_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step2_NI", "", 10),
          numericInput("Mod4Step2_NT", "", 2),
          numericInput("Mod4Step2_NR", "", 10),
          shinyMatrix::matrixInput("Mod4Step2_Vind", value = Mod4Step2updateVind(input, nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod4Step2_Ve", value = matrix(c(input$Mod4Step2_Ve1,   input$Mod4Step2_Corr_e,
                                                       input$Mod4Step2_Corr_e, input$Mod4Step2_Ve2), 2), class = "numeric")
        )
     }),
    outputOptions(output, "Mod4Step2_hidden", suspendWhenHidden = FALSE),
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod4Step2_output <- reactive({
      if (input$Mod4Step2_Run == 0) # if Run button is pressed
        return(NULL)

      isolate({

        updateCheckboxInput(session, "isRunning", value = TRUE)

        # Call app main function
        data <- squid::squidR(input, module = "Mod4Step2")
        
        dt <- as.data.table(data$sampled_data)
        dt <- dt[ , .(Time, Individual, Trait, Phenotype)]
        dt[ , Trait := paste0("Phenotype_", Trait)]
        dt <- dcast(dt, Time + Individual ~ Trait, value.var = "Phenotype")

        updateCheckboxInput(session, "isRunning", value = FALSE)

        return(dt)
      })
    }),
    
    output$Mod4Step2_correlationplot <- renderPlot({

      data  <- Mod4Step2_output()

      if (!is.null(data)) {

        dt <- copy(data)

        ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
          ggplot2::geom_point() +
          ggplot2::xlab("Phenotype of trait y") +
          ggplot2::ylab("Phenotype of trait z") + 
          ggplotCustomTheme()

    	  }else{
    	    defaultPlot()
    	  }

    	}),
    
    output$Mod4Step2_correlationplot2 <- renderPlot({
    	
    	data  <- Mod4Step2_output()
    	
    	if (!is.null(data)) {
    		
    		dt <- copy(data)
    		dt[ , ':='(Phenotype_1 = Phenotype_1 - mean(Phenotype_1), 
    							 Phenotype_2 = Phenotype_2 - mean(Phenotype_2)), by = Individual]
    		
    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Phenotype of trait y") +
    			ggplot2::ylab("Phenotype of trait z") + 
    		  ggplotCustomTheme()
    		
    	}else{
    		defaultPlot()
    	}
    	
    }),

    # Display results
    output$Mod4Step2_Phenotopic_correlation <- renderUI({

      equation <- paste0("$$
                         ",input$Mod4Step1_Corr_e," = 
                         
                         0
                         \\sqrt{
                         (\\frac{0}
                         {0 + ",input$Mod4Step1_Ve1,"})
                         (\\frac{0}
                         {0 + ",input$Mod4Step1_Ve2,"})} + 
                         
                         ",input$Mod4Step1_Corr_e,"
                         \\sqrt{
                         (\\frac{",input$Mod4Step1_Ve1,"}
                         {0 + ",input$Mod4Step1_Ve1,"})
                         (\\frac{",input$Mod4Step1_Ve2,"}
                         {0 + ",input$Mod4Step1_Ve2,"})}"
                         
                         ,"$$")
      
   	  return(withMathJax(equation))
   	}),
    
    output$Mod4Step2_Phenotopic_correlation2 <- renderUI({
    	
    	rpp <- round(input$Mod4Step2_Corr_e * sqrt((input$Mod4Step2_Ve1 / (input$Mod4Step2_Vi1 + input$Mod4Step2_Ve1)) * 
    																			 (input$Mod4Step2_Ve2 / (input$Mod4Step2_Vi2 + input$Mod4Step2_Ve2))), 3)
    	
    	equation <- paste0("$$
    										 ",rpp," = 
    										 
    										 0
    										 \\sqrt{
    										 (\\frac{",input$Mod4Step2_Vi1,"}
    										 {",input$Mod4Step2_Vi1," + ",input$Mod4Step2_Ve1,"})
    										 (\\frac{",input$Mod4Step2_Vi2,"}
    										 {",input$Mod4Step2_Vi2," + ",input$Mod4Step2_Ve2,"})} + 
    										 
    										 ",input$Mod4Step2_Corr_e,"
    										 \\sqrt{
    										 (\\frac{",input$Mod4Step2_Ve1,"}
    										 {",input$Mod4Step2_Vi1," + ",input$Mod4Step2_Ve1,"})
    										 (\\frac{",input$Mod4Step2_Ve2,"}
    										 {",input$Mod4Step2_Vi2," + ",input$Mod4Step2_Ve2,"})}"
    										 
    										 ,"$$")
    	
    	return(withMathJax(equation))
    }),
    
    output$Mod4Step2_Within_Covariance_Matrix <- renderUI({
      
      cov      <- round(input$Mod4Step2_Corr_e * sqrt(input$Mod4Step2_Ve1 * input$Mod4Step2_Ve2),3)
      myMatrix <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
             	    \\begin{pmatrix}
             	    ",input$Mod4Step2_Ve1," & ",cov," \\\\
             	    ",cov," & ",input$Mod4Step2_Ve2," \\\\
             	    \\end{pmatrix} 
             	    $$")
      
      return(withMathJax(myMatrix))
    }),
    
    output$Mod4Step2_Among_Covariance_Matrix <- renderUI({
      
      cov      <- 0
      myMatrix <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
             	    \\begin{pmatrix}
             	    ",input$Mod4Step2_Vi1," & ",cov," \\\\
             	    ",cov," & ",input$Mod4Step2_Vi2," \\\\
             	    \\end{pmatrix} 
             	    $$")
      
      return(withMathJax(myMatrix))
    }),
    
    
    output$Mod4Step2_Repeatabilities <- renderUI({
      
      rep1 <- round(input$Mod4Step2_Vi1 / (input$Mod4Step2_Vi1 + input$Mod4Step2_Ve1), 3)
      rep2 <- round(input$Mod4Step2_Vi2 / (input$Mod4Step2_Vi2 + input$Mod4Step2_Ve2), 3)
      
      rep <- paste0("$$\\text{Repeatability of trait }",NOT$trait.1, "= ",rep1,"$$ 
                     $$\\text{Repeatability of trait }",NOT$trait.2, "= ",rep2,"$$")
      
      return(withMathJax(rep))
    })
) # End return
