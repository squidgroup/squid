#Server functions for module 2 step 3
c(
  ######### Display results (graph) #########
  
  # Display results (table)
  output$Mod2Step3_summary_table <- renderUI({
    
    if(input$Mod2Step3_Run == 0){ # if Run button is pressed
      data <- NULL
    }else{
      
      isolate({ 
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        input_l <- reactiveValuesToList(input)
        
        input_l[["Mod2Step3_Tmax"]] <- 100
        input_l[["Mod2Step3_NI"]]   <- 100
        input_l[["Mod2Step3_NR"]]   <- 5
  
        input_l[["Mod2Step3_B"]]    <- matrix(c(input$Mod2Step3_B0,0,0,0),1)
        
        m <- matrix(rep(0,nb.IS^2),nb.IS)
        diag(m)[1] <- input$Mod2Step3_Vi
        input_l[["Mod2Step3_Vind"]] <- m
        
        input_l[["Mod2Step3_Ve"]]   <- input$Mod2Step3_Vo
        
        
        # Call app main function
        data <- squid::squidR(input_l, module="Mod2Step3")[["sampled_data"]]
        
        data$Phenotype_count <- rpois(nrow(data), exp(data$Phenotype))
        data$Overdispersion  <- as.factor(1:nrow(data))
        
        fit <- lme4::glmer(Phenotype_count ~ 1 + (1|Individual) + (1|Overdispersion), 
                           family = poisson(link="log"), 
                           data   = data)
        
        B0     <- round(lme4::fixef(fit)[1], 2)
        B0_SE  <- round(arm::se.fixef(fit)[1], 2)
        
        RANDEF <- as.data.frame(lme4::VarCorr(fit))$vcov
        Vo     <- round(RANDEF[1], 2)
        Vi     <- round(RANDEF[2], 2)

        updateCheckboxInput(session, "isRunning", value = FALSE)
        
      })
    }
    
    myTable <- data.frame(
      
      "True"       = c("$\\text{Fixed effects}$",
                       paste("Mean of the trait ($",EQ3$mean0,"$) =",round(input$Mod2Step3_B0,2)),
                       "$\\text{Random effects}$",
                       paste("Among-individual variance in intercept ($V_",NOT$devI,"$) =",input$Mod2Step3_Vi),
                       paste("Overdispersion variance ($V_o$) =",input$Mod2Step3_Vo)),
      "Estimated" = c("$\\text{Fixed effects}$",
                      paste("Sampled mean of the trait ($",NOT$mean,"'_0$) =",ifelse(!is.null(data),paste(B0,"\U00b1", B0_SE, sep=" "),"...")),
                      "$\\text{Random effects}$",
                      paste("Sampled among-individual variance in intercept ($V'_",NOT$devI,"$) = ", ifelse(!is.null(data), Vi,"...")),
                      paste("Overdispersion variance of sample ($V'_o$) = ", ifelse(!is.null(data),Vo,"...")))
    ) 
    
    return(getTable(myTable))
  })
  
) # End
