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
      
     actionButton("FMod_runButton", label = FullModel_VAR$Run$label, icon= FullModel_VAR$Run$icon, class="runButton"),
     downloadButton("FMod_download_Rcode", label = FullModel_VAR$download_Rcode$label),
     #      downloadButton("FMod_save_inputs", label = FullModel_VAR$save$label),
#      fileInput("FMod_load_inputs", label = FullModel_VAR$load$label, accept=c(".RData")),
     runningIndicator(),
     p(),
     uiOutput("FMod_runButtonError"),
     p(),
     
     conditionalPanel(
       condition = "0",
       uiOutput('FMod_B_UI_hidden'),
       uiOutput('FMod_Vind_UI_hidden'),
       uiOutput("FMod_Ve_hidden"), 
       uiOutput("FMod_VG_hidden"),
       getCheckboxInput("FMod_X1_state", FullModel_VAR$X1$state),
       getCheckboxInput("FMod_X2_state", FullModel_VAR$X2$state)
     ),
      
     navlistPanel(
       
       well   = TRUE,
       fluid  = FALSE,
       widths = c(3,9),
       # selected = "Variances Summary",
     
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
                       getNumericInput("FMod_NG", FullModel_VAR$NG, "FMod_error_NG")
              )
            )
          )
       ), # End: Simulation design
       
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
           ) # End tabsetPanel
         ) # End wellPanel 
       ), # End: Environment design
       
       # Individual variances design
       tabPanel("Individual variances design",
          wellPanel( 
            h4("Individual variances design"),  
            
            getLabel("FMod_B_UI", FullModel_VAR$B), uiOutput("FMod_B_UI"),
            getLabel("FMod_Vind_UI", FullModel_VAR$Vind), uiOutput("FMod_Vind_UI"), 
            
            fixedRow(
              column(width = 6, getNumericInput("FMod_Ve_input", FullModel_VAR$Ve, "FMod_error_Ve")),
              column(width = 6, getNumericInput("FMod_VG_input", FullModel_VAR$VG, "FMod_error_VG"))
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
       
       # Sampling Design
       tabPanel("Sampling design",
          wellPanel(
            
            h4("Sampling design"),
            fixedRow(
              column(3,
                getNumericInput("FMod_NR", FullModel_VAR$NR, "FMod_error_NR")
              ),
              column(3,
                getLabel("FMod_SampTime_Label", FullModel_VAR$SampTime),
                textOutput("FMod_SampTime")
              )
            ),
            getSliderInput("FMod_Vhsi", FullModel_VAR$Vhsi),
            
            h6("Number of records:", getIcon( FullModel_VAR$Checkbox_NbRecords$infoTxt)),
            getCheckboxInput("FMod_NR_ind", FullModel_VAR$NR_ind),
            getCheckboxInput("FMod_NR_trait", FullModel_VAR$NR_trait),
            
            h6(c("Sampling times:", getIcon(FullModel_VAR$Checkbox_SamTime$infoTxt))),
            getCheckboxInput("FMod_ST_ind", FullModel_VAR$ST_ind),
            getCheckboxInput("FMod_ST_trait", FullModel_VAR$ST_trait),
            
            p(),
            getLabel("FMod_preview_sampling_design_Label", FullModel_VAR$SampDesign_preview),
            p(),
            actionButton("FMod_preview_sampling_design_btn", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon),
            plotOutput("FMod_preview_sampling_design")
          )
       ) # End: Sampling design
     )
  ),# End tabPanel Inputs
  
    # Outputs panel
  tabPanel("Outputs",
  
    actionButton("FMod_rerunButton", label = FullModel_VAR$Run$label, icon= FullModel_VAR$Run$icon, class="runButton"),
    downloadButton("FMod_download_sampled", label = FullModel_VAR$download_sampled$label),
    downloadButton("FMod_download_raw", label = FullModel_VAR$download_raw$label),
    runningIndicator(),
    p(),
    uiOutput("FMod_rerunButtonError"),
    p(),
    
    p(HTML(fullmodelTxt$output_figure_color)),
    
    navlistPanel(   
      fluid  = FALSE,
      widths = c(2,9),
      tabPanel("Environments", plotOutput("FMod_plotEnvironment", height = "1050px")), # END Environements 
      tabPanel("Individual Phenotypes", plotOutput("FMod_plotPhenotype", height = "700px")), # END Individual phenotypes
      tabPanel("Sampling Time", plotOutput("FMod_plotSamples", height = "350px")), # END Environements 
      tabPanel("Data file description",
               tags$b("Full model equation"),
               source("./source/pages/full_model_sbys/ui_fullmodel_equation.R",local=TRUE)[["value"]],
               uiOutput("FMod_Data_Description_Table")
      ) # END Environements 
    )
  ) # END tabpanel Outputs
), # End SimTabsetPanel

HTML("</div>")

)# End fixedPage