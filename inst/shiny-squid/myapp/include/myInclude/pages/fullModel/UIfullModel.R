UIfullModel <- function(){

  # Variables
  pageWidth       <- 12
  sectionWidth    <- pageWidth/2
  subSectionWidth <- (sectionWidth/2)
  
  return(
    
  fixedPage( HTML("<div id='FullModel'>"), 
    h2("Full model simulation (express access)"),
    
    # Inputs and Outputs tabset panel
    tabsetPanel(id = "FMod_TabsetPanel", type = "pills", 
                
      # Inputs panel
      tabPanel("Inputs",
               
       wellPanel(
         h4("Model equation"),
         uiOutput('FMod_myEquations')
       ),         
        
       bsButton("FMod_runButton", label = FullModel_VAR$Run$label, icon= FullModel_VAR$Run$icon, class="runButton", style = FullModel_VAR$Run$style),
       runningIndicator(),
       p(),
       uiOutput("FMod_runButtonError"),        
       p(),
       
       conditionalPanel(
         condition = "0",         
         uiOutput('FMod_B_UI_hidden'),    
         uiOutput('FMod_Vind_UI_hidden'),
         getCheckboxInput("FMod_X1_state", FullModel_VAR$X1$state),
         getCheckboxInput("FMod_X2_state", FullModel_VAR$X2$state)
#          getCheckboxInput("FMod_EG_state", FullModel_VAR$EG$state),
#          getCheckboxInput("FMod_ES_state", FullModel_VAR$ES$state)
       ),  
        
       navlistPanel(  
         
         well   = TRUE,  
         fluid  = FALSE,
         widths = c(3,9),
         # selected = "Variances Summary",
       
#          "Simulation",
       
         # Simulation Design
         tabPanel("Simulation design",
            wellPanel(         
              h4("Simulation design"),
              fixedRow(                     
                column(width = 3,
                       
                       getNumericInput("FMod_Tmax", FullModel_VAR$Tmax, "FMod_error_Tmax") 
                ),
                column(width = 3,
                       getNumericInput("FMod_NP", FullModel_VAR$NP, "FMod_error_NP")                  
                )
              ),
              fixedRow(                     
                column(width = 3,
                       getNumericInput("FMod_NI", FullModel_VAR$NI, "FMod_error_NI")                  
                ),
                column(width = 3,
                       getSelectInput("FMod_NT", FullModel_VAR$NT)
                ),
                column(width = 3,
                         getNumericInput("FMod_NK", FullModel_VAR$NK, "FMod_error_NK") 
                )
              )
            )        
         ), # End: Simulation design
       
#          "Environment",
         
         # Environement design
         tabPanel("Environment design",
                  
           wellPanel(         
             h4("Environment design"),
             tabsetPanel(id = "FMod_Environment_tabsePanel", type = "pills",
                         
                 # X1 ------------------------------------------------------------
                 tabPanel(paste("$",EQ2$env1,"$",sep=""), UIenvironment("FMod", "X1", TRUE)),    
                 
                 # X2 ------------------------------------------------------------
                 tabPanel(paste("$",EQ2$env2,"$",sep=""), UIenvironment("FMod", "X2", TRUE)),
                 
                 # Interaction ------------------------------------------------------------
                 tabPanel(paste("$",EQ2$env12,"$",sep=""), 
                          div(info_msg(FullModel_VAR$X1X2$info)),
                          conditionalPanel(
                            condition = "input.FMod_X1_state == 1 && input.FMod_X2_state == 1",         
                            getCheckboxInput("FMod_X_Interaction", FullModel_VAR$X1X2$state)
                          )
                ) # End Interaction

#                  # EG ------------------------------------------------------------
#                  tabPanel("EG", UIenvironment("FMod", "EG", FALSE)),
#                  
#                  # ES ------------------------------------------------------------
#                  tabPanel("ES", UIenvironment("FMod", "ES", FALSE))
                         
             ) # End tabsetPanel
           ) # End wellPanel 
         ), # End: Environment design
         
         
         # Individual variances design
         tabPanel("Individual variances design",
            wellPanel( 
              h4("Individual variances design"),  
              getLabel("FMod_B_UI", FullModel_VAR$B),
              uiOutput("FMod_B_UI"),
              getLabel("FMod_Vind_UI", FullModel_VAR$Vind),
              uiOutput("FMod_Vind_UI"), 
              fixedRow(                     
                column(width = 6,
                       getNumericInput("FMod_Vme", FullModel_VAR$Vme, "FMod_error_Vme")                   
                ),
                column(width = 6,
                       getNumericInput("FMod_Vk", FullModel_VAR$Vk, "FMod_error_Vk")
                )
              )
              
            )
         ), # End: Individual variances design

        # Variances Summary
        tabPanel("Variances Summary", 
                 wellPanel(
                   
                   h4("Variances Summary"),        
                   uiOutput('FMod_variancesTable')           
                 )
        ), # End: Variances Summary

#          "Sampling",
         
         # Sampling Design
         tabPanel("Sampling design", 
            wellPanel(
              
              h4("Sampling design"),              
              fixedRow(
                column(3,
                  getNumericInput("FMod_NR", FullModel_VAR$NR, "FMod_error_NR")
                ),
                column(3,
                  getLabel("FMod_SampTime", FullModel_VAR$SampTime),
                  textOutput("FMod_SampTime")
                )
              ),
              # getNumericInput("FMod_Vit", FullModel_VAR$Vit, "FMod_error_Vit"),
              getSliderInput("FMod_Vit", FullModel_VAR$Vit),
              
              h6("Number of records:", getIcon("FMod_Checkbox_NbRecords_Info")),
              bsTooltip("FMod_Checkbox_NbRecords_Info", FullModel_VAR$Checkbox_NbRecords$infoTxt, "top"),
              getCheckboxInput("FMod_Drec_Ind", FullModel_VAR$Drec_Ind),
              getCheckboxInput("FMod_Drec_Trait", FullModel_VAR$Drec_Trait),
              
              h6(c("Sampling times:", getIcon("FMod_Checkbox_SamTime_Info"))),
              bsTooltip("FMod_Checkbox_SamTime_Info", FullModel_VAR$Checkbox_SamTime$infoTxt, "top"),
              getCheckboxInput("FMod_Dtime_Ind", FullModel_VAR$Dtime_Ind),
              getCheckboxInput("FMod_Dtime_Trait", FullModel_VAR$Dtime_Trait)              
            )
         ) # End: Sampling design

       )

    ),# End tabPanel Inputs
    
      # Outputs panel
    tabPanel("Outputs",
                       
      bsButton("FMod_rerunButton", label = FullModel_VAR$ReRun$label, icon= FullModel_VAR$ReRun$icon, class="runButton",style = FullModel_VAR$ReRun$style),
      downloadButton("FMod_download_sampled", label = FullModel_VAR$download_sampled$label),
      downloadButton("FMod_download_raw", label = FullModel_VAR$download_raw$label),
      runningIndicator(),
      p(),
      uiOutput("FMod_rerunButtonError"),
      p(),
      navlistPanel(   
        fluid  = FALSE,
        widths = c(2,9),
#         tabPanel("Table", tableOutput("FMod_result")), # END Table 
        tabPanel("Environments", plotOutput("FMod_plotEnvironment", height = "1050px")), # END Environements 
        tabPanel("Individual Phenotypes", plotOutput("FMod_plotPhenotype", height = "700px")), # END Individual phenotypes
        tabPanel("Sampling Time", plotOutput("FMod_plotSamples", height = "350px")), # END Environements 
        tabPanel("Data file description", uiOutput("FMod_Data_Description_Table")) # END Environements 
      )
             
    ) # END tabpanel Outputs
    
  ), # End SimTabsetPanel

  HTML("</div>")

  )# End fixedPage

  ) # End return
  
}