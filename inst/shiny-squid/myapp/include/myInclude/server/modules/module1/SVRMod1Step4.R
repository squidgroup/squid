#Server functions for module 1 step 4
SVRMod1Step4 <- function(input, output, session, color){
  
  return(c(
  
    ######### Set variables #########    
      # Set hidden variables (Tmax, Vind, B, X1_state, X1_shared and X1_ran_V)
      output$Mod1Step4_hidden <- renderUI({
        list(
          numericInput("Mod1Step4_Tmax", "", Modules_VAR$Tmax$max),
          matrixInput2("Mod1Step4_Vind", "",data.frame(matrix(c(input$Mod1Step4_Vi,
                                                                rep(0,(nb.IS*nb.IS)-1)),
                                                              nb.IS))),
          numericInput("Mod1Step4_Vbx","", 1-input$Mod1Step4_Vi-input$Mod1Step4_Vme),
          matrixInput2("Mod1Step4_B", "",data.frame(matrix(c(0,sqrt(1-input$Mod1Step4_Vi-input$Mod1Step4_Vme),0,0),1))),
          checkboxInput("Mod1Step4_X1_state", "", value = TRUE),
          checkboxInput("Mod1Step4_X1_ran_state", "", value = TRUE),
          numericInput("Mod1Step4_X1_ran_V","", 1, min = 0, max = 1, step = 0.001)
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
          data <- main(input, "Mod1Step4", session, TRUE)  
          
          LMR <- lme4::lmer(Phenotype ~ X1 + (1|Individual), data = data$data_S)
          
          FIXEF    <- lme4::fixef(LMR)
          SE.FIXEF <- arm::se.fixef(LMR)
          RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
          
          # Make a mixed effect model to extract variances and slope      
          data$Vp        <- round(var(data$data_S$Phenotype),2)
          data$Vi        <- round(RANDEF[1],2)
          data$Vme       <- round(RANDEF[2],2)
          data$Be1       <- round(FIXEF["X1"],2)
          data$se.Be1    <- round(SE.FIXEF["X1"],2)
          data$mean      <- round(mean(data$data_S$Phenotype),2)
          
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
            
            LM <- lm(Phenotype ~ X1, data = data$data_S)
            
            plot(Phenotype~X1,
                 data = data$data_S,
                 main= bquote(italic(beta[1(estimated)]) == .(data$Be1) %+-% .(data$se.Be1) ~~ (italic(beta[1(true)]) == .(round(input$Mod1Step4_B[1,2],2)))),
                 xlab="Environment", 
                 ylab="Phenotype",
                 pch = 19,
                 col = color$color2) 
            abline(LM)          
        
          })
          
        }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
                
      }),

      # Graph: Individual reaction norm  
      output$Mod1Step4_plot2 <- renderPlot({ 
      
        data      <- Mod1Step4_output()
        
        if(!is.null(data)){
          
          isolate({ 
                  
#             plot(Phenotype~X1,
#                  data = data$data_S,
#                  main= bquote(V[.(NOT$devI)] == .(data$Vi)),
#                  xlab="Environment", 
#                  ylab="Phenotype per individual",
#                  pch = 19,
#                  col = Individual) 
            
            xyplot(Phenotype~X1,
                   type="b",
                   data=data$data_S,
                   group=Individual,
                   xlab="Environment", 
                   ylab="Phenotype per individual"
            )
            
          })
          
        }else{ plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n") }
      }),

      # Table : display true and measured values (Vp, Vme, mean and Beta es)
      output$Mod1Step4_summary_table <- renderUI({ 
        
        data <- Mod1Step4_output()
        
        myTable <- data.frame("True"       = c(paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod1Step4_Vi),
                                               paste("Residual variance ($V_",NOT$error,"$) =",input$Mod1Step4_Vme),
                                               "Mean of the trait ($\\mu$) = 0",
                                               paste("Slope of environmental effect ($",EQ3$mean1,"$) =",round(input$Mod1Step4_B[1,2],2))),
                              "Estimated" = c(paste("Individual variance in sample ($V'_",NOT$devI,"$) = "      ,ifelse(!is.null(data),data$Vi,"...")),
                                              paste("Residual variance of sample ($V'_",NOT$residual,"$) = "        ,ifelse(!is.null(data),data$Vme,"...")),
                                              paste("Sampled mean of the trait ($\\mu'$) = "        ,ifelse(!is.null(data),data$mean,"...")),
                                              paste("Estimated slope of environmental effect ($",NOT$mean,"'_1$) =  "    ,ifelse(!is.null(data),paste(data$Be1,"\U00b1", data$se.Be1, sep=" "),"..."))) 
                             )  
        
        getTable(myTable)
    
      }), 	
 	
    
    ######### Manage errors #########
      observe({
        if(
#          !testInput(input$Mod1Step4_NI, Modules_VAR$NI, TRUE, FALSE)  || 
#          !testInput(input$Mod1Step4_Vi, Modules_VAR$Vi, FALSE, FALSE) ||
#          !testInput(input$Mod1Step4_Vme, Modules_VAR$Vme, FALSE, FALSE) || 
         !testInput(input$Mod1Step4_Vbx, Modules_VAR$Vb1x1, FALSE, FALSE) #||
#          !testInput(input$Mod1Step4_NR, Modules_VAR$NR, TRUE, FALSE) ||  
#          !testInput(input$Mod1Step4_Bx1, Modules_VAR$Bes, FALSE, FALSE)
         ){
          updateButton(session, "Mod1Step4_Run", disabled = TRUE, style = Modules_VAR$Run$invalidStyle)
        }else{
          updateButton(session, "Mod1Step4_Run", disabled = FALSE, style = Modules_VAR$Run$style)
        }
      }),
  
#       output$Mod1Step4_error_NI   <- renderUI({testInput(input$Mod1Step4_NI, Modules_VAR$NI, TRUE, TRUE)}),
#       output$Mod1Step4_error_Vi   <- renderUI({testInput(input$Mod1Step4_Vi, Modules_VAR$Vi, FALSE, TRUE)}),
#       output$Mod1Step4_error_Vme  <- renderUI({testInput(input$Mod1Step4_Vme, Modules_VAR$Vi, FALSE, TRUE)}),
      output$Mod1Step4_error_Vbx   <- renderUI({testInput(input$Mod1Step4_Vbx, Modules_VAR$Vb1x1, FALSE, TRUE)})#,
#       output$Mod1Step4_error_NR   <- renderUI({testInput(input$Mod1Step4_NR, Modules_VAR$NR, TRUE, TRUE)}),
#       output$Mod1Step4_error_Be1  <- renderUI({testInput(input$Mod1Step4_Be1, Modules_VAR$Bes, FALSE, TRUE)})
                
  )) # End return
}


