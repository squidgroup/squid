#Server functions for module 4 step 3
c(
    ######### Set variables #########    
    # Set hidden variables
    Mod4Step3updateVind <- function(input, nb.IS){
      m <- matrix(rep(0,(nb.IS*2)^2),(nb.IS*2))
      diag(m)[1]         <- input$Mod4Step3_Vi1
      diag(m)[1 + nb.IS] <- input$Mod4Step3_Vi2
      m[nb.IS + 1,1]     <- input$Mod4Step3_Corr_I
      return(m)
    },
     output$Mod4Step3_hidden <- renderUI({
        list(
          numericInput("Mod4Step3_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step3_NI", "", 10),
          numericInput("Mod4Step3_NT", "", 2),
          numericInput("Mod4Step3_NR", "", 10),
          shinyMatrix::matrixInput("Mod4Step3_Vind", value = Mod4Step3updateVind(input, nb.IS), class = "numeric"),
          shinyMatrix::matrixInput("Mod4Step3_Ve", value = matrix(c(input$Mod4Step3_Ve1,   input$Mod4Step3_Corr_e,
                                                        input$Mod4Step3_Corr_e, input$Mod4Step3_Ve2), 2), class = "numeric")
        )
     }),
    outputOptions(output, "Mod4Step3_hidden", suspendWhenHidden = FALSE),
    
    ######### Run simulation #########
    # Run simulation and return results
    Mod4Step3_output <- reactive({
      if (input$Mod4Step3_Run == 0) # if Run button is pressed
        return(NULL)

      isolate({

        updateCheckboxInput(session, "isRunning", value = TRUE)

        # Call app main function
        data <- squid::squidR(input, module = "Mod4Step3")
       
        dt <- as.data.table(data$sampled_data)
        dt <- dt[ , .(Time, Individual, Trait, Phenotype)]
        dt[ , Trait := paste0("Phenotype_", Trait)]
        dt <- dcast(dt, Time + Individual ~ Trait, value.var = "Phenotype")

        updateCheckboxInput(session, "isRunning", value = FALSE)

        return(dt)
      })
    }),
    
    output$Mod4Step3_correlationplot <- renderPlot({

      data  <- Mod4Step3_output()

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
    
    output$Mod4Step3_correlationplot2 <- renderPlot({

    	data  <- Mod4Step3_output()

    	if (!is.null(data)) {

    		dt <- copy(data)
    		dt <- dt[ , .(Phenotype_1 = mean(Phenotype_1),
    					      	Phenotype_2 = mean(Phenotype_2)), by = Individual]

    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Mean phenotype of trait y") +
    			ggplot2::ylab("Mean Phenotype of trait z") + 
    		  ggplotCustomTheme()

    	}else{
    		defaultPlot()
    	}

    }),
    
    
    output$Mod4Step3_correlationplot3 <- renderPlot({
    	
    	data  <- Mod4Step3_output()
    	
    	if (!is.null(data)) {
    		
    		dt <- copy(data)
    		dt[ , ':='(Phenotype_1 = Phenotype_1 - mean(Phenotype_1), 
    							 Phenotype_2 = Phenotype_2 - mean(Phenotype_2)), by = Individual]
    		
    		ggplot2::ggplot(dt, ggplot2::aes(x = Phenotype_1, Phenotype_2,  fill = as.factor(Individual), colour = as.factor(Individual))) +
    			ggplot2::geom_point() +
    			ggplot2::xlab("Deviation from individual phenotype mean of trait y") +
    			ggplot2::ylab("Deviation from individual phenotype mean of trait y") + 
    		  ggplotCustomTheme()
    		
    	}else{
    		defaultPlot()
    	}
    	
    }),

    # Display results
    output$Mod4Step3_Phenotopic_correlation <- renderUI({
    	
    	rpp <-  input$Mod4Step3_Corr_I * sqrt((input$Mod4Step3_Vi1 / (input$Mod4Step3_Vi1 + input$Mod4Step3_Ve1)) * 
    																					(input$Mod4Step3_Vi2 / (input$Mod4Step3_Vi2 + input$Mod4Step3_Ve2))) +
    					input$Mod4Step3_Corr_e * sqrt((input$Mod4Step3_Ve1 / (input$Mod4Step3_Vi1 + input$Mod4Step3_Ve1)) * 
    																				(input$Mod4Step3_Ve2 / (input$Mod4Step3_Vi2 + input$Mod4Step3_Ve2)))
    	
      equation <- paste0("$$
                         ",round(rpp, 2)," =

                         ",input$Mod4Step3_Corr_I,"
                         \\sqrt{
                         (\\frac{",input$Mod4Step3_Vi1 ,"}
                         {",input$Mod4Step3_Vi1 ," + ",input$Mod4Step3_Ve1,"})
                         (\\frac{",input$Mod4Step3_Vi2,"}
                         {",input$Mod4Step3_Vi2," + ",input$Mod4Step3_Ve2,"})} +

                         ",input$Mod4Step3_Corr_e,"
                         \\sqrt{
                         (\\frac{",input$Mod4Step3_Ve1,"}
                         {",input$Mod4Step3_Vi1," + ",input$Mod4Step3_Ve1,"})
                         (\\frac{",input$Mod4Step3_Ve2,"}
                         {",input$Mod4Step3_Vi2," + ",input$Mod4Step3_Ve2,"})}"

                         ,"$$")

   	  return(withMathJax(equation))
   	}),
    
    output$Mod4Step3_Within_Covariance_Matrix <- renderUI({
      
      cov      <- round(input$Mod4Step3_Corr_e * sqrt(input$Mod4Step3_Ve1 * input$Mod4Step3_Ve2),3)
      myMatrix <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
             	    \\begin{pmatrix}
             	    ",input$Mod4Step3_Ve1," & ",cov," \\\\
             	    ",cov," & ",input$Mod4Step3_Ve2," \\\\
             	    \\end{pmatrix} 
             	    $$")
      
      return(withMathJax(myMatrix))
    }),
    
    output$Mod4Step3_Among_Covariance_Matrix <- renderUI({

    	cov      <- round(input$Mod4Step3_Corr_I * sqrt(input$Mod4Step3_Vi1 * input$Mod4Step3_Vi2),3)
      myMatrix <- paste0(
        "$$ \\Omega_{",NOT$error,"}=
             	    \\begin{pmatrix}
             	    ",input$Mod4Step3_Vi1," & ",cov," \\\\
             	    ",cov," & ",input$Mod4Step3_Vi2," \\\\
             	    \\end{pmatrix}
             	    $$")
      return(withMathJax(myMatrix))
    }),
    
    output$Mod4Step3_Repeatabilities <- renderUI({

      rep1 <- round(input$Mod4Step3_Vi1 / (input$Mod4Step3_Vi1 + input$Mod4Step3_Ve1), 3)
      rep2 <- round(input$Mod4Step3_Vi2 / (input$Mod4Step3_Vi2 + input$Mod4Step3_Ve2), 3)

      rep <- paste0("$$\\text{Repeatability of trait }",NOT$trait.1, "= ",rep1,"$$
                     $$\\text{Repeatability of trait }",NOT$trait.2, "= ",rep2,"$$")

      return(withMathJax(rep))
    })
) # End return
