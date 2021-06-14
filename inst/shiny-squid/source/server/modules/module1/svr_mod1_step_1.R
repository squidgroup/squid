#Server functions for module 1 step 1
c(
    ######### Set hidden variables #########    
      output$Mod1Step1_hidden <- renderUI({
        # Intercepts and slopes (Co)variance matrix
        shinyMatrix::matrixInput("Mod1Step1_Vind", value=matrix(c(1-input$Mod1Step1_Ve,rep(0,(nb.IS*nb.IS)-1)),nb.IS), class = "numeric")
      }),
      outputOptions(output, "Mod1Step1_hidden", suspendWhenHidden = FALSE),

    ######### Run simulation #########
      Mod1Step1_output <- reactive({
      if(input$Mod1Step1_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({     
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function      
        data <- squid::squidR(input, module="Mod1Step1") 
        
        data$Vp              <- round(var(data$sampled_data$Phenotype),2)
        data$phenotypeMean   <- round(mean(data$sampled_data$Phenotype),2)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),    
    
    ######### Display results (graph) #########
      # Graph: density distribution of true and measured phenotypic values
      output$Mod1Step1_plot <- renderPlot({ 
        
        data  <- Mod1Step1_output()
        
        if(!is.null(data)){
          
          mydata    <- data.frame(dens = c(data$sampled_data$Phenotype, data$sampled_data$I)
                                  , lines = rep(c(paste("Total phenotype (",NOT$trait.1,")",sep=""), 
                                                  paste("Individual phenotype (",NOT$devI,")",sep="")), each = length(data$sampled_data$Phenotype)))
          
          ggplot2::ggplot(mydata, ggplot2::aes(dens, fill = lines, colour = lines)) +
    											ggplot2::geom_density(alpha = 0.1) +
    											ggplot2::geom_rug(ggplot2::aes(col=lines)) +
    											ggplot2::ggtitle("Distribution of total and individual phenotype values") +
    											ggplot2::xlab("Phenotype values") +
    											ggplot2::ylab("Density") +
                          ggplot2::theme(legend.title    = ggplot2::element_blank(),
                                         legend.position = "bottom")
          
        }else{
          defaultPlot()
        }
        
      }),
      
      # Text : display true and measured values (Vp, Vi and mean)   
      output$Mod1Step1_summary_table <- renderUI({ 
        
          data <- Mod1Step1_output()
        
          myTable <- data.frame(
            "True"= c(paste("Total Phenotypic variance ($V_",NOT$total,"$) = 1",sep=""),
                      paste("Individual Variance ($V_",NOT$devI,"$) = ",1-input$Mod1Step1_Ve,sep=""),
                      paste("Measurement error variance ($V_",NOT$mError,"$) = ",input$Mod1Step1_Ve,sep=""),
                      "mean of the trait ($\\mu$) = 0"),
            "Estimated" = c(paste("Total Sampled Phenotypic variance ($V'_",NOT$total,"$) = ",ifelse(!is.null(data),data$Vp,"..."),sep=""),
                            "",
                            "",
                            paste("Sampled mean of the trait ($\\mu'$) = ",ifelse(!is.null(data),data$phenotypeMean,"..."),sep=""))
            )
        
          getTable(myTable)
      }) 
            
) # End
