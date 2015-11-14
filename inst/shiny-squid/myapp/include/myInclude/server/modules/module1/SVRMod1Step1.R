#Server functions for module 1 step 1
SVRMod1Step1 <- function(input, output, session, color){
  
  return(c(
    
    ######### Set hidden variables #########    
      output$Mod1Step1_hidden <- renderUI({
        # Intercepts and slopes (Co)variance matrix
        matrixInput2("Mod1Step1_Vind", "",data.frame(matrix(c(1-input$Mod1Step1_Vme,rep(0,(nb.IS*nb.IS)-1)),nb.IS)))
      }),
      outputOptions(output, "Mod1Step1_hidden", suspendWhenHidden = FALSE),

    ######### Run simulation #########
      Mod1Step1_output <- reactive({
      if(input$Mod1Step1_Run == 0) # if Run button is pressed
        return(NULL)
      
      isolate({     
        
        updateCheckboxInput(session, "isRunning", value = TRUE)
        
        # Call app main function      
        data <- main(input, "Mod1Step1", session, TRUE) 
        
        data$Vp        <- round(var(data$data_S$Phenotype),2)
        data$Vj        <- round(var(data$data_S$J0),2)
        data$mean      <- round(mean(data$data_S$Phenotype),2)
        
        updateCheckboxInput(session, "isRunning", value = FALSE)
        
        return(data)
      })  
    }),    
    
    ######### Display results (graph) #########
      # Graph: density distribution of true and measured phenotypic values
      output$Mod1Step1_plot <- renderPlot({ 
        
        data  <- Mod1Step1_output()          
        
        if(!is.null(data)){                    
          
          mydata    <- data.frame(dens = c(data$data_S$Phenotype, data$data_S$J0)
                                  , lines = rep(c(paste("Total phenotype (",NOT$trait.1,")",sep=""), 
                                                  paste("Individual phenotype (",NOT$devI,")",sep="")), each = length(data$data_S$Phenotype)))
          
          print(densityplot(~dens, data=mydata, groups=lines,
                            plot.points=TRUE, ref=TRUE, 
                            col=c(color$color1, color$color2),
                            par.settings = list(superpose.line = list(col=c(color$color1, color$color2))),
                            auto.key = list(corner = c(0.95, 0.95)),
                            main="Distribution of total and individual phenotype values",
                            xlab="Phenotype values",
                            ylab="Density"))            
          
        }else{
          print(plot(0,type='n',ann=FALSE, xaxt = "n", yaxt = "n"))
        }
        
      }),
      
      # Text : display true and measured values (Vp, Vi and mean)   
      output$Mod1Step1_summary_table <- renderUI({ 
        
          myTable <- data.frame(
            "True"= c(paste("Total Phenotypic variance ($V_",NOT$total,"$) = 1",sep=""),
                      paste("Individual Variance ($V_",NOT$devI,"$) = ",1-input$Mod1Step1_Vme,sep=""),
                      paste("Residual variance ($V_",NOT$error,"$) = ",input$Mod1Step1_Vme,sep=""),
                      "mean of the trait ($\\mu$) = 0"),
            "Estimated" = c(paste("Total Sampled Phenotypic variance ($V'_",NOT$total,"$) = ",ifelse(!is.null(Mod1Step1_output()),Mod1Step1_output()$Vp,"..."),sep=""),
                            "",
                            "",
                            paste("Sampled mean of the trait ($\\mu'$) = ",ifelse(!is.null(Mod1Step1_output()),Mod1Step1_output()$mean,"..."),sep=""))
            )
        
          getTable(myTable)
      }) 

    ######### Manage errors #########
      # display error message and disable button if so
#       observe({
#         if(!testInput(input$Mod1Step1_NI, Modules_VAR$NI, TRUE, FALSE) || 
#              !testInput(input$Mod1Step1_Vme, Modules_VAR$Vme, FALSE, FALSE)){
#           updateButton(session, "Mod1Step1_Run", disabled = TRUE, style = Modules_VAR$Run$invalidStyle)
#         }else{
#           updateButton(session, "Mod1Step1_Run", disabled = FALSE, style = Modules_VAR$Run$style)
#         }
#       }),
#       
#       output$Mod1Step1_error_NI  <- renderUI({testInput(input$Mod1Step1_NI, Modules_VAR$NI, TRUE, TRUE)}),
#       output$Mod1Step1_error_Vme <- renderUI({testInput(input$Mod1Step1_Vme, Modules_VAR$Vme, FALSE, TRUE)})
            
  )) # End return
}