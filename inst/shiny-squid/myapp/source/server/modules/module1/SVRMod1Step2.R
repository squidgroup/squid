#Server functions for module 1 step 2
c(
  
    ######### Set hidden variables #########    
      # Set hidden variable (Tmax and Vi)
      output$Mod1Step2_hidden <- renderUI({
        list(
          numericInput("Mod1Step2_Tmax", "", Modules_VAR$Tmax$max),
          matrixInput2("Mod1Step2_Vind", "",data.frame(matrix(c(1-input$Mod1Step2_Vme,rep(0,(nb.IS*nb.IS)-1)),nb.IS)))
        )
      }),
      outputOptions(output, "Mod1Step2_hidden", suspendWhenHidden = FALSE),
        
    ######### Run simulation #########
      Mod1Step2_output <- reactive({
        if(input$Mod1Step2_Run == 0) # if Run button is pressed
          return(NULL)
        
        isolate({
          
          updateCheckboxInput(session, "isRunning", value = TRUE)
          
          # Call app main function
          data <- SQUID::runSQUIDfct(input, "Mod1Step2") 
          
          LMR      <- lme4::lmer(Phenotype ~ 1 + (1|Individual), data = data$sampled_Data)
          RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
          
          data$Vi            <- round(RANDEF[1],2)
          data$Vme           <- round(RANDEF[2],2)
          data$Vp            <- data$Vi + data$Vme
          data$phenotypeMean <- round(mean(data$sampled_Data$Phenotype),2)
          data$R             <- round(data$Vi / data$Vp,2)
          
          updateCheckboxInput(session, "isRunning", value = FALSE)
          
          return(data)
        })  
      }),
    
    ######### Display results (graph) #########
      # Display results (graph)
      output$Mod1Step2_plot <- renderPlot({ 
        
        data      <- Mod1Step2_output()
        
        if(!is.null(data)){
          
          Vp        <- paste("V'",NOT$total," = " , data$Vp)
          Vi        <- paste("V'",NOT$devI, " = " , data$Vi)
          Vme       <- paste("V'",NOT$error," = ", data$Vme)
          
          myFactor  <- factor(rep(c(Vp,Vi,Vme), each=length(data$sampled_Data$Phenotype)), levels=c(Vp,Vi,Vme))                
          mydata    <- data.frame(dens  = c(data$sampled_Data$Phenotype,data$sampled_Data$I, data$sampled_Data$e),
                                  lines = myFactor)
          
          print(densityplot(~dens|lines,data=mydata,
                            plot.points = T,
                            xlab="Model component values",
                            ylab="Density"))
          
        }else{
          print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
        }
                
      }),
      
      # Display results (table)
      output$Mod1Step2_summary_table <- renderUI({ 
        
        myTable <- data.frame("True"     = c(paste("Total phenotypic variance ($V_",NOT$total,"$) = 1"),
                                            paste("Individual variance ($V_",NOT$devI,"$) =",1-input$Mod1Step2_Vme),
                                            paste("Residual variance ($V_",NOT$error,"$) =",input$Mod1Step2_Vme),
                                            "Mean of the trait ($\\mu$) = 0"),
                              "Estimated"= c(paste("Total Sampled Phenotypic variance ($V'_",NOT$total,"$) = ",ifelse(!is.null(Mod1Step2_output()),Mod1Step2_output()$Vp,"...")),
                                             paste("Sampled Individual variance ($V'_",NOT$devI,"$) = ",ifelse(!is.null(Mod1Step2_output()),Mod1Step2_output()$Vi,"...")),
                                             paste("Residual variance of sample ($V'_",NOT$residual,"$) = ",ifelse(!is.null(Mod1Step2_output()),Mod1Step2_output()$Vme,"...")),
                                             paste("Sampled mean of the trait ($\\mu'$) = ",ifelse(!is.null(Mod1Step2_output()),Mod1Step2_output()$phenotypeMean,"...")))
                              )
                  
        getTable(myTable)
        
      }),
      
      # display results: repeatability (text)
      output$Mod1Step2_Rep_txt   <- renderText({ HTML(paste("Your repeatability is $",NOT$repeatability,"$ =", ifelse(!is.null(Mod1Step2_output()), 
                                                                                                Mod1Step2_output()$R,"...")))}),
      # Display repeatability result (graph)
      output$Mod1Step2_plot2 <- renderPlot({ 
        
        if(!is.null(Mod1Step2_output())){
          
          data         <- Mod1Step2_output()$sampled_Data
          phen_time1   <- subset(data, data$Time == data$Time[1], select=Phenotype)
          phen_time2   <- subset(data, data$Time == data$Time[2], select=Phenotype)
          
          plot(phen_time2$Phenotype~phen_time1$Phenotype, 
               xlab="First measurement", 
               ylab="Second measurement",
               pch = 19,
               col = color$color2)
          
        }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
      })
            
  ) # End return