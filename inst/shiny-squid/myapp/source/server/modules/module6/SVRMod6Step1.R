#Server functions for module 6 step 1
c(
  
    ######### Set variables #########  
    Mod6Step1updateVind <- function(input, nb.IS){
      df <- matrix(rep(0,nb.IS^2),nb.IS)
      diag(df)[1] <- input$Mod6Step1_Vi
      diag(df)[2] <- input$Mod6Step1_Vs
      return(as.data.frame(df))
    },
      # Set hidden variables
       output$Mod6Step1_hidden <- renderUI({
          list(
            numericInput("Mod6Step1_Tmax", "", Modules_VAR$Tmax$max),
            matrixInput2("Mod6Step1_Vind", "", Mod6Step1updateVind(input, nb.IS)),
            matrixInput2("Mod6Step1_B", "",data.frame(matrix(c(0,sqrt(input$Mod6Step1_Vbx),0,0),1))),
            checkboxInput("Mod6Step1_X1_state", "", value = TRUE),
            checkboxInput("Mod6Step1_X1_ran_state", "", value = TRUE),
            checkboxInput("Mod6Step1_X1_ran_shared", "", value = TRUE),
            numericInput("Mod6Step1_X1_ran_V","", 1, min = 0, max = 1, step = 0.001),
            checkboxInput("Mod6Step1_Dtime_Ind", "", value = TRUE)
          )
        }),
 	    outputOptions(output, "Mod6Step1_hidden", suspendWhenHidden = FALSE),

    # Run simulation and return results
    Mod6Step1_output <- reactive({
      
      if(input$Mod6Step1_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- SQUID::runSQUIDfct(input, "Mod6Step1")  
        
        LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual) + (0+X1|Individual), data = data$sampled_Data)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi        <- round(RANDEF[1],2)
        data$Vs        <- round(RANDEF[2],2)
        data$Vr        <- round(RANDEF[3],2) 
        data$B0        <- round(lme4::fixef(LMR)[1],2) 
        data$B1        <- round(lme4::fixef(LMR)[2],2) 
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    
    Mod6Step1_table <- reactive({
      
      data    <- Mod6Step1_output()
      
      myTable <- data.frame(
        "True"       = c("$\\text{Fixed effects}$",
                        paste("Mean of the trait ($",EQ3$mean0,"$) =",round(input$Mod6Step1_B[1],2)),
                        paste("Population-specific slope of the environmental effect ($",EQ3$mean1,"$) =",round(input$Mod6Step1_B[2],2)),
                        "$\\text{Random effects}$",
                        paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod6Step1_Vi),
                        paste("Individual-specific response to an environmental effect (random slopes) variance ($V_",NOT$devS,"$) =",input$Mod6Step1_Vs),
                        paste("Residual variance ($V_",NOT$error,"$) =",input$Mod6Step1_Ve)),
        "Estimated" = c("$\\text{Fixed effects}$",
                        paste("Sampled mean of the trait ($",NOT$mean,"'_0$) =",ifelse(!is.null(data),data$B0,"...")),
                        paste("Estimated population-specific slope of the environmental effect ($",NOT$mean,"'_1$) =",ifelse(!is.null(data),data$B1,"...")),
                        "$\\text{Random effects}$",
                        paste("Individual variance in sample ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi,"...")),
                        paste("Individual-specific response to an environmental effect (random slopes) variance ($V'_",NOT$devS,"$) = ", ifelse(!is.null(data),data$Vs,"...")),
                        paste("Residual variance of sample ($V'_",NOT$residual,"$) = ", ifelse(!is.null(data),data$Vr,"...")))
      )  
      
      return(getTable(myTable))
    }),
    
    # Display results (table)
    output$Mod6Step1_summary_table <- renderUI({Mod6Step1_table()}),
    
    
    output$Mod6Step1_plot <- renderPlot({ 
      
      data  <- Mod6Step1_output()          
      
      if(!is.null(data)){                    
        
        print(
          ggplot2::ggplot(data = data$sampled_Data, ggplot2::aes(y     = Phenotype, 
                                                           x     = X1, 
                                                           color = as.factor(Individual))) +
                          ggplot2::stat_smooth(method = "lm", se=FALSE) + 
                          ggplot2::theme(legend.position="none") + 
                          ggplot2::xlab("Environmental effect") + 
                          ggplot2::ylab("Phenotype"))
        
      }else{
        print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
      }
      
    })
    

  ) # End return