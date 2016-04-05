#Server functions for module 1 step 4
c(
  
    ######### Set variables #########    
      # Set hidden variables (Tmax, Vind, B, X1_state, X1_shared and X1_sto_V)
      Mod1Step4updateB <- function(input){
        B <- sqrt(1-input$Mod1Step4_Vi-input$Mod1Step4_Ve)
        return(ifelse(is.nan(B),0,B))
      },
      output$Mod1Step4_hidden <- renderUI({
        list(
          numericInput("Mod1Step4_Tmax", "", Modules_VAR$Tmax$max),
          matrixInput2("Mod1Step4_Vind", "",data.frame(matrix(c(input$Mod1Step4_Vi,
                                                                rep(0,(nb.IS*nb.IS)-1)),
                                                              nb.IS))),
          numericInput("Mod1Step4_Vbx","", 1-input$Mod1Step4_Vi-input$Mod1Step4_Ve),
          matrixInput2("Mod1Step4_B", "",data.frame(matrix(c(0,Mod1Step4updateB(input),0,0),1))),
          checkboxInput("Mod1Step4_X1_state", "", value = TRUE),
          checkboxInput("Mod1Step4_X1_sto_state", "", value = TRUE),
          numericInput("Mod1Step4_X1_sto_V","", 1, min = 0, max = 1, step = 0.001)
        )
      }),
      outputOptions(output, "Mod1Step4_hidden", suspendWhenHidden = FALSE),
  
    # display variable (known environmental effect Vbx)
    output$Mod1Step4_Vbx_txt <- renderUI({
      
      if(!testInput(input$Mod1Step4_Vbx, Modules_VAR$Vb1x1, FALSE, FALSE)){
        output <- span(strong(round(input$Mod1Step4_Vbx,2),class="alert alert-danger"))
      }else{
        output <- span(round(input$Mod1Step4_Vbx,2))
      }
      
      p(HTML(paste(strong(Modules_VAR$Vb1x1$label),output,"")))
      
    }),
      
    ######### Run simulation #########
      Mod1Step4_output <- reactive({
        if(input$Mod1Step4_Run == 0) # if Run button is pressed
          return(NULL)
        
        isolate({          
          
          updateCheckboxInput(session, "isRunning", value = TRUE)

          # Call app main function
          data <- SQUID::squidR(input, module="Mod1Step4")
          
          LMR <- lme4::lmer(Phenotype ~ X1 + (1|Individual), data = data$sampled_Data)
          
          FIXEF    <- lme4::fixef(LMR)
          SE.FIXEF <- arm::se.fixef(LMR)
          RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
          
          # Make a mixed effect model to extract variances and slope      
          data$Vp            <- round(var(data$sampled_Data$Phenotype),2)
          data$Vi            <- round(RANDEF[1],2)
          data$Ve           <- round(RANDEF[2],2)
          data$B1            <- round(FIXEF["X1"],2)
          data$se.B1         <- round(SE.FIXEF["X1"],2)
          data$phenotypeMean <- round(mean(data$sampled_Data$Phenotype),2)
          
          updateCheckboxInput(session, "isRunning", value = FALSE)
          
          return(data)
        })  
      }),
   	
    ######### Display results (graph) #########
      # Graph: Individual phenotypes over environment  
      output$Mod1Step4_plot1 <- renderPlot({       
        
        data      <- Mod1Step4_output()
        
        if(!is.null(data)){
          
          isolate({ 

            ggplot2::ggplot(data$sampled_Data, ggplot2::aes(x     = X1, 
                                                            y     = Phenotype)) +
              ggplot2::geom_point() + 
              ggplot2::geom_smooth(method = "lm", se = FALSE) + 
              ggplot2::xlab("Environment") +
              ggplot2::ylab("Phenotype") + 
              ggplot2::ggtitle(bquote(italic(beta[1(estimated)]) == .(data$B1) %+-% .(data$se.B1) ~~ (italic(beta[1(true)]) == .(round(input$Mod1Step4_B[1,2],2)))))
        
          })
          
        }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
                
      }),

      # Graph: Individual reaction norm  
      output$Mod1Step4_plot2 <- renderPlot({ 
      
        data      <- Mod1Step4_output()
        
        if(!is.null(data)){
          
          isolate({ 
            ggplot2::ggplot(data$sampled_Data, ggplot2::aes(x     = X1, 
                                                            y     = Phenotype, 
                                                            color = as.factor(Individual),
                                                            group = as.factor(Individual))) +
              ggplot2::geom_point() + 
              ggplot2::geom_smooth(method = "lm", se = FALSE) + 
              ggplot2::xlab("Environment") +
              ggplot2::ylab("Phenotype per individual") + 
              ggplot2::theme(legend.position="none")

          })
          
        }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
      }),

      # Table : display true and measured values (Vp, Ve, mean and Beta es)
      output$Mod1Step4_summary_table <- renderUI({ 
        
        data <- Mod1Step4_output()
        
        myTable <- data.frame("True"       = c(paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod1Step4_Vi),
                                               paste("Residual variance ($V_",NOT$error,"$) =",input$Mod1Step4_Ve),
                                               "Mean of the trait ($\\mu$) = 0",
                                               paste("Slope of environmental effect ($",EQ3$mean1,"$) =",round(input$Mod1Step4_B[1,2],2))),
                              "Estimated" = c(paste("Individual variance in sample ($V'_",NOT$devI,"$) = "      ,ifelse(!is.null(data),data$Vi,"...")),
                                              paste("Residual variance of sample ($V'_",NOT$residual,"$) = "        ,ifelse(!is.null(data),data$Ve,"...")),
                                              paste("Sampled mean of the trait ($\\mu'$) = "        ,ifelse(!is.null(data),data$phenotypeMean,"...")),
                                              paste("Estimated slope of environmental effect ($",NOT$mean,"'_1$) =  "    ,ifelse(!is.null(data),paste(data$B1,"\U00b1", data$se.B1, sep=" "),"..."))) 
                             )  
        
        getTable(myTable)
    
      }), 	
    
    ######### Manage errors #########
      observe({
        if(!testInput(input$Mod1Step4_Vbx, Modules_VAR$Vb1x1, FALSE, FALSE)){
          disableActionButton("Mod1Step4_Run", session, "true")
        }else{
          disableActionButton("Mod1Step4_Run", session, "false")
        }
      }),
      output$Mod1Step4_error_Vbx   <- renderUI({testInput(input$Mod1Step4_Vbx, Modules_VAR$Vb1x1, FALSE, TRUE)})
                
  ) # End return