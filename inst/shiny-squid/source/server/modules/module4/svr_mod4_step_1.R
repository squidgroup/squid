#Server functions for module 4 step 1
c(
    ######### Set variables #########    
    # Set hidden variables
     output$Mod4Step1_hidden <- renderUI({
        list(
          numericInput("Mod4Step1_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step1_NI", "", 10),
          numericInput("Mod4Step1_NT", "", 2),
          numericInput("Mod4Step1_NR", "", 10),
          shinyMatrix::matrixInput("Mod4Step1_Ve", value = matrix(c(input$Mod4Step1_Ve1,   input$Mod4Step1_Corr_e, 
                                                       input$Mod4Step1_Corr_e, input$Mod4Step1_Ve2), 2), class = "numeric")
        )
     }),
    outputOptions(output, "Mod4Step1_hidden", suspendWhenHidden = FALSE),
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod4Step1_output <- reactive({
      if (input$Mod4Step1_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- squid::squidR(input, module = "Mod4Step1")
        
        dt <- as.data.table(data$sampled_data)
        dt <- dt[ , .(Time, Individual, Trait, Phenotype)]
        dt[ , Trait := paste0("Phenotype_", Trait)]
        dt <- dcast(dt, Time + Individual ~ Trait, value.var = "Phenotype")
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(dt)
      })
    }),
    
    output$Mod4Step1_correlationplot <- renderPlot({
      
      data  <- Mod4Step1_output()
      
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

    # Display results (Matrix)
    output$Mod4Step1_Covariance_Matrix <- renderUI({
      
      cov      <- round(input$Mod4Step1_Corr_e * sqrt(input$Mod4Step1_Ve1 * input$Mod4Step1_Ve2),3)
      myMatrix <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
             	    \\begin{pmatrix}
             	    ",input$Mod4Step1_Ve1," & ",cov," \\\\
             	    ",cov," & ",input$Mod4Step1_Ve2," \\\\
             	    \\end{pmatrix} 
             	    $$")
      
   	  return(withMathJax(myMatrix))
   	})
) # End return
