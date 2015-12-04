#Server functions for module 6 step 2
SVRMod6Step2 <- function(input, output, session, Modules_VAR, nb.IS, color){
  
  return(c(
  
    ######### Set variables #########  
    Mod6Step2updateVind <- function(input, nb.IS){
      df <- matrix(rep(0,nb.IS^2),nb.IS)
      diag(df)[1] <- input$Mod6Step2_Vi
      diag(df)[2] <- input$Mod6Step2_Vs
      df[2,1]     <- input$Mod6Step2_CorIS
      return(as.data.frame(df))
    },
      # Set hidden variables
       output$Mod6Step2_hidden <- renderUI({
          list(
            numericInput("Mod6Step2_Tmax", "", Modules_VAR$Tmax$max),
            matrixInput2("Mod6Step2_Vind", "", Mod6Step2updateVind(input, nb.IS)),
            matrixInput2("Mod6Step2_B", "",data.frame(matrix(c(0,input$Mod6Step2_beta1,0,0),1))),
            checkboxInput("Mod6Step2_X1_state", "", value = TRUE),
            checkboxInput("Mod6Step2_X1_ran_state", "", value = TRUE),
            checkboxInput("Mod6Step2_X1_ran_shared", "", value = TRUE),
            numericInput("Mod6Step2_X1_ran_V","", 1, min = 0, max = 1, step = 0.001),
            checkboxInput("Mod6Step2_Dtime_Ind", "", value = FALSE)
          )
      }),
 	    outputOptions(output, "Mod6Step2_hidden", suspendWhenHidden = FALSE),

    # Run simulation and return results
    Mod6Step2_output <- reactive({
      
      if(input$Mod6Step2_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function
        data <- main(input, "Mod6Step2", session, TRUE)  
        
        LMR      <- lme4::lmer(Phenotype ~ X1 + (X1|Individual), data = data$data_S)
        RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
        
        data$Vi        <- round(RANDEF[1],2)
        data$Vs        <- round(RANDEF[2],2)
        data$CorIS     <- round(RANDEF[3]/sqrt(RANDEF[1]*RANDEF[2]),2)
        data$Vr        <- round(RANDEF[4],2) 
        data$B0        <- round(fixef(LMR)[1],2) 
        data$B1        <- round(fixef(LMR)[2],2) 
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),
    
    Mod6Step2_table <- reactive({
      
      data    <- Mod6Step2_output()
      
      myTable <- data.frame(
        "True"       = c("$\\text{Fixed effects}$",
                        paste("Mean of the trait ($",EQ3$mean0,"$) =",round(input$Mod6Step2_B[1],2)),
                        paste("Population-specific slope of the environmental effect ($",EQ3$mean1,"$) =",round(input$Mod6Step2_B[2],2)),
                        "$\\text{Random effects}$",
                        paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod6Step2_Vi),
                        paste("Individual-specific response to an environmental effect (random slopes) variance ($V_",NOT$devS,"$) =",input$Mod6Step2_Vs),
                        paste("Correlation between individual specific intercepts and slopes ($Cor_{",NOT$devI,",",NOT$devS,"}$) =",input$Mod6Step2_CorIS),
                        paste("Residual variance ($V_",NOT$error,"$) =",input$Mod6Step2_Vme)),
        "Estimated" = c("$\\text{Fixed effects}$",
                        paste("Sampled mean of the trait ($",NOT$mean,"'_0$) =",ifelse(!is.null(data),data$B0,"...")),
                        paste("Estimated population-specific slope of the environmental effect ($",NOT$mean,"'_1$) =",ifelse(!is.null(data),data$B1,"...")),
                        "$\\text{Random effects}$",
                        paste("Individual variance in sample ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi,"...")),
                        paste("Individual-specific response to an environmental effect (random slopes) variance ($V'_",NOT$devS,"$) = ", ifelse(!is.null(data),data$Vs,"...")),
                        paste("Correlation between individual specific intercepts and slopes ($Cor_{",NOT$devI,",",NOT$devS,"}$) =", ifelse(!is.null(data),data$CorIS,"...")),
                        paste("Residual variance of sample ($V'_",NOT$residual,"$) = ", ifelse(!is.null(data),data$Vr,"...")))
      )  
      
      return(getTable(myTable))
    }),
    
    # Display results (table)
    output$Mod6Step2_summary_table <- renderUI({Mod6Step2_table()})
    
#     output$Mod6Step2_plot <- renderPlot({ 
#       
#       data  <- Mod6Step2_output()          
#       
#       if(!is.null(data)){                    
#         
#         print(ggplot(data = data$data_S, aes(y=Phenotype, x=X1, color=as.factor(Individual))) +
#           stat_smooth(method = "lm", se=FALSE) + 
#           theme(legend.position="none") + 
#           xlab("Environmental effect") + 
#           ylab("Phenotype"))
#         
#       }else{
#         print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
#       }
#       
#     })
    

  )) # End return
}