#Server functions for module 4 step 2
c(
    ######### Set variables #########    
    # Set hidden variables
    Mod4Step2updateVind <- function(input, nb.IS){
      df <- matrix(rep(0,(nb.IS*2)^2),(nb.IS*2))
      diag(df)[1]         <- input$Mod4Step2_Vi1
      diag(df)[1 + nb.IS] <- input$Mod4Step2_Vi2
      return(as.data.frame(df))
    },
     output$Mod4Step2_hidden <- renderUI({
        list(
          numericInput("Mod4Step2_Tmax", "", Modules_VAR$Tmax$max),
          numericInput("Mod4Step2_NI", "", 10),
          numericInput("Mod4Step2_NT", "", 2),
          numericInput("Mod4Step2_NR", "", 10),
          matrixInput2("Mod4Step2_Vind", "", Mod4Step2updateVind(input, nb.IS)),
          matrixInput2("Mod4Step2_Ve", "", data.frame(matrix(c(input$Mod4Step2_Ve1,   input$Mod4Step2_Corr_e,
                                                               input$Mod4Step2_Corr_e, input$Mod4Step2_Ve2), 2)))
        )
     }),
    outputOptions(output, "Mod4Step2_hidden", suspendWhenHidden = FALSE),
    
    ######### Run simulation #########
    # Run simulation and return results
    # Mod4Step2_output <- reactive({
    #   if (input$Mod4Step2_Run == 0) # if Run button is pressed
    #     return(NULL)
    #   
    #   isolate({
    #     
    #     updateCheckboxInput(session, "isRunning", value = TRUE)
    #     
    #     # Call app main function
    #     data <- squid::squidR(input, module = "Mod4Step1")
    #     
    #     updateCheckboxInput(session, "isRunning", value = FALSE)
    #     
    #     return(data)
    #   })
    # }),
    
    # output$Mod4Step2_correlationplot <- renderPlot({
    #   
    #   data  <- Mod4Step2_output()
    #   
    #   if (!is.null(data)) {
    #     
    #     dt <- as.data.table(data$sampled_data)
    #     dt <- dt[ , .(Time, Individual, Trait, Phenotype)]
    #     dt[ , Trait := paste0("Trait_", Trait)]
    #     dt <- dcast(dt, Time + Individual ~ Trait, value.var = "Phenotype")
    #     
    #     ggplot2::ggplot(dt, ggplot2::aes(x = Trait_1, Trait_2,  fill = Individual, colour = Individual)) +
    #       ggplot2::geom_point() +
    #       ggplot2::xlab("Phenotype of trait y") +
    #       ggplot2::ylab("Phenotype of trait z") + 
    #       ggplot2::theme(legend.position = "none")
    # 	    
    # 	  }else{
    # 	    print(plot(0,type = 'n',ann = FALSE, xaxt = "n", yaxt = "n"))
    # 	  }
    # 	  
    # 	}),

    # Display results (Matrix)
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
