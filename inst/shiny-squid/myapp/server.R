
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Source included packages and functions
source("include/include.R")

# Initialisation ------------------------------------------

# Graphs
color <- list(
  "color1"  = "red",
  "color2"  =  "dodgerblue"
)

data_S      <- NULL
environment <- NULL
sliderMin   <- NULL
sliderMax   <- NULL
TmaxOld     <- 100
Test        <- NULL

isPreparing <- FALSE
isRunning   <- FALSE


# My WebServer  ----------------------------------------------------------------

shinyServer(function(input, output, session) {
  
 

  # Portal ----------------
  
  # Modules   ---------------------------------------------------------  
  SVRmodules(input, session)  
  
    # Module 1  ---------------------------------------------------------  
  
    # Module 1 Step 1 ---------------------------------------------------------  
      SVRMod1Step1(input, output, session, color)
    
    # Module 1 Step 2 ---------------------------------------------------------
      SVRMod1Step2(input, output, session, color)
    
    # Module 1 Step 3 ---------------------------------------------------------
      SVRMod1Step3(input, output, session, color)

    # Module 1 Step 4 ---------------------------------------------------------
      SVRMod1Step4(input, output, session, color)

  
  # Module 3  --------------------------------------------------------- 
  
    # Module 3 Step 1 ---------------------------------------------------------
    SVRMod3Step1(input, output, session, Modules_VAR, nb.IS, color)
  
    # Module 3 Step 2 ---------------------------------------------------------
    SVRMod3Step2(input, output, session, Modules_VAR, FullModel_VAR, nb.IS, color)
  
    # Module 3 Step 3 ---------------------------------------------------------
    SVRMod3Step3(input, output, session, Modules_VAR, FullModel_VAR, nb.IS, color)
  
  # Module 6  --------------------------------------------------------- 
  
    # Module 6 Step 1 ---------------------------------------------------------
    SVRMod6Step1(input, output, session, Modules_VAR, nb.IS, color)
  
    SVRMod6Step2(input, output, session, Modules_VAR, nb.IS, color)
  
  
  
  #     output$table_test <- renderTable({ 
  #       if(!is.null(Mod1Step3_output())){
  #         Mod1Step3_output()$data_S
  #       }else{
  #         data.frame(0)
  #       }
  #     })
  
  # Full model  (Step by Step)   ----------------
    SVRFullModel("FModSbyS", input, output, session)  
  
  # Full model  (express access) ----------------
    SVRFullModel("FMod", input, output, session)
  
# #   output$Sim_myEquation1 <- renderUI({
# 
#     # Equation trait 1
#     myEqu_1 <- paste(
#       "$$Y_{ij}=", paste( 
#         # Interecept
#         ifelse(input$Sim_B[1,1]    != 0 | input$Sim_Vind[1,1] != 0, "(", ""),  
#         ifelse(input$Sim_B[1,1]    != 0, "b_0", ""),
#         ifelse(input$Sim_B[1,1]    != 0 & input$Sim_Vind[1,1] != 0, "+", ""),
#         ifelse(input$Sim_Vind[1,1] != 0, "I_{0j}", ""),
#         ifelse(input$Sim_B[1,1]    != 0 | input$Sim_Vind[1,1] != 0, ")+", ""), 
#         
#         # Slope 1 (X1)
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & (input$Sim_B[1,2] > 0 | input$Sim_Vind[2,2] > 0), "(", ""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_B[1,2] > 0, "b_1",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_B[1,2] > 0 & input$Sim_Vind[2,2] > 0, "+",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_Vind[2,2] > 0, "I_{1j}",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & (input$Sim_B[1,2] > 0 | input$Sim_Vind[2,2] > 0), ")X_1+", ""),
#         
#         # Slope 2 (X2)
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & (input$Sim_B[1,3] > 0 | input$Sim_Vind[3,3] > 0), "(", ""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_B[1,3]    > 0, "b_2",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_B[1,3]    > 0 & input$Sim_Vind[3,3] > 0, "+",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_Vind[3,3] > 0, "I_{2j}",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & (input$Sim_B[1,3] > 0 | input$Sim_Vind[3,3] > 0), ")X_2+", ""),  
#         
#         # Slope 3 (X1X2)
#         ifelse(input$Sim_X_Interaction != 0 & (input$Sim_B[1,4] > 0 | input$Sim_Vind[4,4] > 0), "(", ""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_B[1,4] > 0, "b_3",""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_B[1,4] > 0 & input$Sim_Vind[4,4] > 0, "+",""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_Vind[4,4] > 0, "I_{3j}",""),
#         ifelse(input$Sim_X_Interaction != 0 & (input$Sim_B[1,4] > 0 | input$Sim_Vind[4,4] > 0), ")X_1X_2+", ""),
#         
#         ifelse(input$Sim_EG_state != 0 & input$Sim_EG_ran_V > 0, "EG+", ""),
#         ifelse(input$Sim_ES_state != 0 & input$Sim_ES_ran_V > 0, "ES+", ""),
#         
#         ifelse(input$Sim_Vme > 0,"ME_{ij}",""),sep=""),
#       "$$", sep="")
#     
#     if(substr(myEqu_1, nchar(myEqu_1)-2, nchar(myEqu_1)) == "+$$") myEqu_1 <- paste(substr(myEqu_1, 1, nchar(myEqu_1)-3),"$$", sep="")    
#     
#     list(withMathJax2(myEqu_1))
#   })
# 
#   # Equation  trait 2
#   output$Sim_myEquation2 <- renderUI({
#     
#     if(input$Sim_NT < 2) return()
#     
#     myEqu_2 <- paste(
#       "$$Z_{ij}=", paste( 
#         # Interecept
#         ifelse(input$Sim_B[1,nb.IS+1]    != 0 || input$Sim_Vind[nb.IS+1,nb.IS+1] != 0, "(", ""),  
#         ifelse(input$Sim_B[1,nb.IS+1]    != 0, "b_0", ""),
#         ifelse(input$Sim_B[1,nb.IS+1]    != 0 & input$Sim_Vind[nb.IS+1,nb.IS+1] != 0, "+", ""),
#         ifelse(input$Sim_Vind[nb.IS+1,nb.IS+1] != 0, "I_{0j}", ""),
#         ifelse(input$Sim_B[1,nb.IS+1]    != 0 || input$Sim_Vind[nb.IS+1,nb.IS+1] != 0, ")+", ""), 
#         
#         # Slope 1 (X1)
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & (input$Sim_B[1,nb.IS+2] > 0 || input$Sim_Vind[nb.IS+2,nb.IS+2] > 0), "(", ""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_B[1,nb.IS+2] > 0, "b_1",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_B[1,nb.IS+2] > 0 & input$Sim_Vind[nb.IS+2,nb.IS+2] > 0, "+",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & input$Sim_Vind[nb.IS+2,nb.IS+2] > 0, "I_{1j}",""),
#         ifelse(input$Sim_X1_state != 0 & input$Sim_X1_ran_V > 0 & (input$Sim_B[1,nb.IS+2] > 0 || input$Sim_Vind[nb.IS+2,nb.IS+2] > 0), ")X_1+", ""),
#         
#         # Slope 2 (X2)
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & (input$Sim_B[1,nb.IS+3] > 0 || input$Sim_Vind[nb.IS+3,nb.IS+3] > 0), "(", ""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_B[1,nb.IS+3]    > 0, "b_2",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_B[1,nb.IS+3]    > 0 & input$Sim_Vind[nb.IS+3,nb.IS+3] > 0, "+",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & input$Sim_Vind[nb.IS+3,nb.IS+3] > 0, "I_{2j}",""),
#         ifelse(input$Sim_X2_state != 0 & input$Sim_X2_ran_V > 0 & (input$Sim_B[1,nb.IS+3] > 0 || input$Sim_Vind[nb.IS+3,nb.IS+3] > 0), ")X_2+", ""),  
#         
#         # Slope 3 (X1X2)
#         ifelse(input$Sim_X_Interaction != 0 & (input$Sim_B[1,nb.IS+4] > 0 || input$Sim_Vind[nb.IS+4,nb.IS+4] > 0), "(", ""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_B[1,nb.IS+4] > 0, "b_3",""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_B[1,nb.IS+4] > 0 & input$Sim_Vind[nb.IS+4,nb.IS+4] > 0, "+",""),
#         ifelse(input$Sim_X_Interaction != 0 & input$Sim_Vind[nb.IS+4,nb.IS+4] > 0, "I_{3j}",""),
#         ifelse(input$Sim_X_Interaction != 0 & (input$Sim_B[1,nb.IS+4] > 0 || input$Sim_Vind[nb.IS+4,nb.IS+4] > 0), ")X_1X_2+", ""),
#         
#         ifelse(input$Sim_EG_state != 0 & input$Sim_EG_ran_V > 0, "EG+", ""),
#         ifelse(input$Sim_ES_state != 0 & input$Sim_ES_ran_V > 0, "ES+", ""),
#         
#         ifelse(input$Sim_Vme > 0,"ME_{ij}",""),sep=""),
#       "$$", sep="")
#     
#     if(substr(myEqu_2, nchar(myEqu_2)-2, nchar(myEqu_2)) == "+$$") myEqu_2 <- paste(substr(myEqu_2, 1, nchar(myEqu_2)-3),"$$", sep="")
#     
#     list(withMathJax2(helpText(myEqu_2)))
#   })
# 
#   output$Sim_B_UI <- renderUI({
#     
# #     NT <- as.numeric(input$Sim_NT)
# #     
# #     inputOn <- matrix(rep(c(TRUE,
# #                             ifelse(input$Sim_X1_state,TRUE,FALSE),
# #                             ifelse(input$Sim_X2_state,TRUE,FALSE),
# #                             ifelse(input$Sim_X_Interaction,TRUE,FALSE)),NT),1)
# #     isolate({
# #       if("Sim_B" %in% names(input)){ 
# #         B            <- matrix(rep(0,NT*nb.IS),1)      
# #         newSize      <- ifelse(NT*nb.IS > length(input$Sim_B),length(input$Sim_B),NT*nb.IS)      
# #         B[1:newSize] <- input$Sim_B[1:newSize]   
# #         
# #       }else{ B <- Sim_B_init}
# #     })
# #     
# #     B              <- data.frame(B)
# #     colnames(B)    <- rep(c("B.0", "B1.X1", "B2.X2", "B3.X1X2"), NT) 
# #     
# #     matrixInputB("Sim_B", c("Population mean values", getIcon("Info")), B, inputOn, NT)
#   })
#   
#   output$Sim_Vind_UI <- renderUI({
#     
# #     NT <- as.numeric(input$Sim_NT)    
# #     
# #     inputOn <- matrix(rep(TRUE,(nb.IS*NT)^2),nb.IS*NT)
# #     
# #     if(!input$Sim_X1_state){
# #       inputOn[seq(from=2,to=(NT*nb.IS),by=nb.IS),] <- FALSE
# #       inputOn[,seq(from=2,to=(NT*nb.IS),by=nb.IS)] <- FALSE
# #     }
# #     if(!input$Sim_X2_state){ 
# #       inputOn[seq(from=3,to=(NT*nb.IS),by=nb.IS),]  <- FALSE
# #       inputOn[,seq(from=3,to=(NT*nb.IS),by=nb.IS)] <- FALSE
# #     }
# #     if(!input$Sim_X_Interaction){ 
# #       inputOn[seq(from=4,to=(NT*nb.IS),by=nb.IS),] <- FALSE
# #       inputOn[,seq(from=4,to=(NT*nb.IS),by=nb.IS)] <- FALSE
# #     }
# #     
# #     isolate({
# #       if("Sim_Vind" %in% names(input)){ 
# #         Vind                       <- matrix(rep(0,(nb.IS*NT)^2),nb.IS*NT) 
# #         Vind[1:nb.IS, 1:nb.IS]     <- input$Sim_Vind[1:nb.IS, 1:nb.IS]
# #       }else{ 
# #         Vind           <- Sim_Vind_init 
# #       }
# #     })
# #     
# #     Vind           <- data.frame(Vind)
# #     colnames(Vind) <- rep(c("I.0", "I1.X1", "I2.X2", "I3.X1X2"), NT)  
# #     
# #     matrixInputVind("Sim_Vind", c("Individuals (Co)Variance matrix", getIcon("Info")), Vind, inputOn, NT)
#   })
# 
#   observe({
#     if(!input$Sim_X1_state || !input$Sim_X2_state)
#       updateCheckboxInput(session, "Sim_X_Interaction", value = FALSE)
#   })
# 
# 
#   getSamplingInterval <- function(){
#     
#     max  <- input$Sim_Tmax # Total time
#     time <- input$Sim_Time_sampling  # Sampling time interval in percentage   
#     
#     min_value <- floor((time[1] / 100) * max)
#     max_value <- floor((time[2] / 100) * max)
#     
#     return(c(min_value, max_value))
#   }
#   
#   
#   # Calculate sampling time interval 
#   output$Sim_Time_sampling_text <- renderText({
#   
#     result <- getSamplingInterval()
#     print(paste("From",result[1],"to", result[2]))
#       
#   })
#   
#   # Update sampling time checkbox relative to the records checkbox
#   observe({
#     if(input$Sim_Dtime_Ind)   updateCheckboxInput(session, "Drec_Ind",   value = TRUE)
#     if(input$Sim_Dtime_Trait) updateCheckboxInput(session, "Drec_Trait", value = TRUE)
#     
#     if(!input$Sim_Drec_Ind & input$Sim_Dtime_Ind)       updateCheckboxInput(session, "Sim_Drec_Ind",   value = TRUE)
#     if(!input$Sim_Drec_Trait & input$Sim_Dtime_Trait)   updateCheckboxInput(session, "Sim_Drec_Trait", value = TRUE)  
#   })
#   
#   
#   # Update max records number when the sampling time changes
#   
#   observe({
#     
#     result <- getSamplingInterval()
#     
#     updateNumericInput(session, "Sim_NR", max = (result[2] - result[1]))
#   })
#   
#   # Reset the environment data 
#   observe({
#     
#     input$Sim_Tmax
#     input$Sim_environmentType
#     input$Sim_ran_Mu
#     input$Sim_ran_V
#     input$Sim_lin_Intercep
#     input$Sim_lin_slope
#     input$Sim_auto_Mu
#     input$Sim_auto_V
#     environment <<- NULL
# 
#   })
#   
#   # Display environment data
#   output$environnementPlot <- renderPlot({
#     
#     input$Sim_Tmax
#     input$Sim_environmentType
#     input$Sim_ran_Mu
#     input$Sim_ran_V
#     input$Sim_lin_Intercep
#     input$Sim_lin_slope
#     input$Sim_auto_Mu
#     input$Sim_auto_V
#     
#     if(input$Sim_env_preview){
#        
#       if(is.null(environment)){                                   
#         
#         source("functions/__get_environment.R")
#         
#         Env <- list(
#           "type" = input$environmentType,
#           "ran"  = list("Mu" = input$ran_Mu,"V" = input$ran_V),
#           "lin"  = list("Intercept" = input$lin_Intercep,"Slope" = input$lin_slope),
#           "auto" = list("Mu"=input$auto_Mu,"V" = input$auto_V)
#         )
#          
#         environment <<- get_environment(Env, input$Sim_Tmax)
#    
#       }
#       
#       print(plot(1:input$Sim_Tmax, environment, ylab="Environment value", xlab="Time"))
#       
#     }
# 
#   })
#   
#   
#   #make dynamic slider
#   #   output$slider <- renderUI({
#   #     sliderInput("inSlider", "Slider", min=input$min_val, max=input$max_val, value=2000)
#   #   })
# 
#   
#   # Switch to the output panel when the app is runned
#   observe({
#     if(input$Sim_runButton != 0 | input$Sim_rerunButton != 0) updateTabsetPanel(session, "SimTabsetPanel", selected = "Outputs")
#   })
#   
#   mySimulation <- reactive({ 
#     
#     if(input$Sim_rerunButton == 0 & input$Sim_runButton == 0  ) # if Run button is pressed 
#       return(NULL)
#     
#     isolate({ 
#       
#       # Call app main function
#       data <- main(input, "Sim", session, progress) 
#       
#       # save data
#       data_S <<- data$data_S 
#             
#       return(data)
#       
#     })              
#   })
# 
# #   output$Sim_result <- renderTable({ 
# #     
# #       data <- mySimulation() 
# #       
# #       LMR <- lmer(Phenotype ~ X1 + (1 + X1|individual), data = data$data_S)
# #       
# # #       as.data.frame(VarCorr(LMR))
# # 
# #         V <- as.data.frame(VarCorr(LMR))
# #         P <- print(VarCorr(LMR),comp=c("Variance","Std.Dev."))
# # 
# #         Group <- c("Individual", "", "Residual") 
# #         Name <- c("Random Intercept (I.0)", "Random slope (I.X1)", "")
# # 
# #         True.Variance <- c(data$V$Vind[1,1], data$V$Vind[2,2], data$V$Vm) 
# #         Estimate.Variance <- c(V$vcov[1], V$vcov[2], V$vcov[4])         
# #         True.Corr <- c("",round(data$V$Vind[2,1],digits = 2),"")
# #         Corr <- c("",round(attr(P$individual, "correlation")[1,2], digits = 2),"")
# #         
# #         data.frame(Group, Name, True.Variance, Estimate.Variance, True.Corr,Corr) 
# #     
# #   })
# 
#   output$Sim_PlotEnvironment <- renderPlot({
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 0, visible = TRUE)    
#     data <- mySimulation() 
#     updateProgressBar(session, inputId = "Sim_PB", value = 70)
#     
#     #   print result graphs 
#     print(multiplot(data$myPlot$plotX1,                    
#                     data$myPlot$plotX2,
#                     data$myPlot$plotEG,
#                     data$myPlot$plotES,
#                     cols=1))
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 100)
#     updateProgressBar(session, inputId = "Sim_PB", visible = FALSE)
#     
#   })
#   
#   output$Sim_PlotPhenotype <- renderPlot({
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 0, visible = TRUE)    
#     data <- mySimulation() 
#     updateProgressBar(session, inputId = "Sim_PB", value = 70)
#     
#     #   print result graphs 
#     print(multiplot(data$myPlot$plotTotPhen,                    
#                     data$myPlot$plotSampPhen,                    
#                     cols=1))
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 100)
#     updateProgressBar(session, inputId = "Sim_PB", visible = FALSE)
#     
#   })
#   
#   output$Sim_PlotSamples <- renderPlot({
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 0, visible = TRUE)    
#     data <- mySimulation() 
#     updateProgressBar(session, inputId = "Sim_PB", value = 70)
#     
#     #   print result graphs 
#     print(data$myPlot$plotSampTime)
#     
#     updateProgressBar(session, inputId = "Sim_PB", value = 100)
#     updateProgressBar(session, inputId = "Sim_PB", visible = FALSE)
#     
#   })
#   
#   # Run app
# #   output$myPlot <- renderPlot({
# #     
# #     if(input$rerunButton != 0 | input$runButton != 0  ){ # if Run button is pressed   
# #       
# #       # Verify total variance is equal to 1
# # #       totV <- input$VindInput[1] + input$VindInput[length(input$VindInput)] + input$VtInput[1] + input$VeInput
# # #       validate(
# # #         need(totV == 1, "The total variance must be equal to 1.")
# # #       )
# #       
# #       # Initialize progress bar
# #       progress <- Progress$new(session)
# #       progress$set(message = 'Simulation', value = 0)
# #       
# #       # Call app main function
# #       data <- main(input, session, progress) 
# #       
# #       progress$set(value = 1)
# #       progress$close()
# #       
# #       # save data
# #       data_S <<- data$data_S
# #       
# #       # print result graphs 
# #       print(multiplot(data$myPlot$plot1,
# #                       data$myPlot$plot2,
# #                       data$myPlot$plot3,
# #                       cols=1))
# # #       
# # #       max  <- input$Tmax
# # #       time <- input$Time_sampling    
# # #       
# # #       min_value <- floor((time[1] / 100) * max)
# # #       max_value <- floor((time[2] / 100) * max)
# # #       print(plot(1:input$Tmax, data$data_C$x[1:input$Tmax]))
# #       
# #     } 
# #     
# #   })
#   
#   # Download results file
#   output$Sim_downloadButton <- downloadHandler(
#     filename = paste('samplingData', '.csv', sep=''),
#     content = function(file) {
#       write.csv(data_S, file)
#     }
#   )
  

})
