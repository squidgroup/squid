
# Model input page
span( 
  
  conditionalPanel(
    condition = "0",
    uiOutput('FModSbyS_B_UI_hidden'),
    uiOutput('FModSbyS_Vind_UI_hidden'),
    getCheckboxInput("FModSbyS_X1_state", FullModel_VAR$X1$state),
    getCheckboxInput("FModSbyS_X2_state", FullModel_VAR$X2$state)
  ), 
  
  fixedPage( 
    
    p(HTML(fullmodelTxt$inputStepbyStepDesign)),
    
    h4("Simulation design"),
    p(HTML(fullmodelTxt$inputSimDesign)),
    
    fixedRow(
      column(width = 9,
             wellPanel( 
               
               fixedRow(
                 column(width = 3,
                
                        getNumericInput("FModSbyS_Tmax", FullModel_VAR$Tmax, "FModSbyS_error_Tmax")
                 ),
                 column(width = 3,
                        getNumericInput("FModSbyS_NP", FullModel_VAR$NP, "FModSbyS_error_NP")
                 )
               ),
               fixedRow(
                 column(width = 3,
                        getNumericInput("FModSbyS_NI", FullModel_VAR$NI, "FModSbyS_error_NI")
                 ),
                 column(width = 3,
                        getSelectInput("FModSbyS_NT", FullModel_VAR$NT)
                 ),
                 column(width = 3,
                        getNumericInput("FModSbyS_NG", FullModel_VAR$NG, "FModSbyS_error_NG")
                 )
               )
         )
      )
    ),
    
    h4("Environment design"),
    
    p(HTML(fullmodelTxt$inputEnvironment_1)),
    p(HTML(fullmodelTxt$inputEnvironment_2)),
    p(HTML(fullmodelTxt$inputEnvironment_3)),
    p(HTML(fullmodelTxt$inputEnvironment_4)),
    p(HTML(fullmodelTxt$inputEnvironment_5)),
    p(HTML(fullmodelTxt$inputEnvironment_6)),
    p(HTML(fullmodelTxt$inputEnvironment_7)),
    
    # Environment
    fixedRow(
      column(width = 9,
       wellPanel(  
         tabsetPanel(id = "FModSbyS_Environment_tabsePanel_1", type = "pills",
                     
                     # X1 ------------------------------------------------------------
                     tabPanel(paste("$",EQ2$env1,"$",sep=""), UIenvironment("FModSbyS", "X1", TRUE)),    
                     
                     # X2 ------------------------------------------------------------
                     tabPanel(paste("$",EQ2$env2,"$",sep=""), UIenvironment("FModSbyS", "X2", TRUE)),
                     
                     # Interaction ------------------------------------------------------------
                     tabPanel(paste("$",EQ2$env12,"$",sep=""), 
                              div(info_msg(FullModel_VAR$X1X2$info)),
                              conditionalPanel(
                                condition = "input.FModSbyS_X1_state == 1 && input.FModSbyS_X2_state == 1",         
                                getCheckboxInput("FModSbyS_X_Interaction", FullModel_VAR$X1X2$state)
                              )
                     ) # End Interaction
                     
         ) # End tabsetPanel
       )
      )
    ),
    
    h4("Model equation"),    
    p(HTML(fullmodelTxt$inputEquation)),
    fixedRow(
      column(width = 9,
             wellPanel(uiOutput('FModSbyS_myEquations'))
      )
    ),
    
    h4("Individual phenotypes design"),
    p(HTML(fullmodelTxt$inputPhenDesign)),
    fixedRow(
      column(width = 9,
             wellPanel(             
               getLabel("FModSbyS_B_UI", FullModel_VAR$B),
               uiOutput("FModSbyS_B_UI"),
               getLabel("FModSbyS_Vind_UI", FullModel_VAR$Vind),
               uiOutput("FModSbyS_Vind_UI"), 
               fixedRow(                     
                 column(width = 6,
                        getNumericInput("FModSbyS_Ve_input", FullModel_VAR$Ve, "FModSbyS_error_Ve"),
                        conditionalPanel(
                          condition = "0",
                          uiOutput("FModSbyS_Ve_hidden")
                        )
                 ),
                 column(width = 6,
                        getNumericInput("FModSbyS_VG_input", FullModel_VAR$VG, "FModSbyS_error_VG"),
                        conditionalPanel(
                          condition = "0",
                          uiOutput("FModSbyS_VG_hidden")
                        )
                 )
               )
             )
      )
    ),
    
    h4("Variances Summary"),
    p(HTML(fullmodelTxt$inputVarSummary)),
    fixedRow(
      column(width = 9,
       wellPanel(
         uiOutput('FModSbyS_variancesTable')
       )
      )
    ),
    
    h4("Sampling design"),
    p(HTML(fullmodelTxt$inputSamplingDesign_1)),
    p(HTML(fullmodelTxt$inputSamplingDesign_2)),
    p(HTML(fullmodelTxt$inputSamplingDesign_3)),
    p(HTML(fullmodelTxt$inputSamplingDesign_4)),
    fixedRow(
      column(width = 9,
       wellPanel(
         fixedRow(
           column(width = 6,
                  fixedRow(
                    column(6,
                           getNumericInput("FModSbyS_NR", FullModel_VAR$NR, "FModSbyS_error_NR")
                    ),
                    column(6,
                           getLabel("FModSbyS_SampTime_Label", FullModel_VAR$SampTime),
                           textOutput("FModSbyS_SampTime")
                    )
                  ),
                  getSliderInput("FModSbyS_Vhsi", FullModel_VAR$Vhsi)
           ),
           column(width = 6,
                  h6("Number of records:", getIcon(FullModel_VAR$Checkbox_NbRecords$infoTxt)),
                  getCheckboxInput("FModSbyS_NR_ind", FullModel_VAR$NR_ind),
                  getCheckboxInput("FModSbyS_NR_trait", FullModel_VAR$NR_trait),
                  h6(c("Sampling times:", getIcon(FullModel_VAR$Checkbox_SamTime$infoTxt))),
                  getCheckboxInput("FModSbyS_ST_ind", FullModel_VAR$ST_ind),
                  getCheckboxInput("FModSbyS_ST_trait", FullModel_VAR$ST_trait)  
           )
         ),
         
         p(),
         getLabel("FModSbyS_preview_sampling_design_Label", FullModel_VAR$SampDesign_preview),
         p(),
         actionButton("FModSbyS_preview_sampling_design_btn", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon),
         plotOutput("FModSbyS_preview_sampling_design")
         
       )
      )
    ),
    
    p(HTML(fullmodelTxt$inputRun)),
    p(HTML(fullmodelTxt$inputRcode)),
    actionButton("FModSbyS_runButton", label = FullModel_VAR$Run$label, icon= FullModel_VAR$Run$icon, class="runButton"),
    downloadButton("FModSbyS_download_Rcode", label = FullModel_VAR$download_Rcode$label),
    # downloadButton("FModSbyS_save", label = FullModel_VAR$save$label),
    runningIndicator(),
    p(),
    uiOutput("FModSbyS_runButtonError"),
    p("")
  )
)