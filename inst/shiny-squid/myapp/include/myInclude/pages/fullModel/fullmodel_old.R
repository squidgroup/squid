fullmodel_old <- function(){

  # Variables
  pageWidth       <- 12
  sectionWidth    <- pageWidth/2
  subSectionWidth <- (sectionWidth/2)
  
  # source text
  source("www/text/fullmodel/fullmodel.R")
  
  return(
    
  fixedPage( HTML("<div id='FullModel'>"), 
    h2("Full simulation model"),
    
#     p(HTML(fullmodel.txt$parag.intro)),
    
    # Inputs and Outputs tabset panel
    tabsetPanel(id = "FullModelTabsetPanel", type = "tabs",
                
      # Inputs panel
      tabPanel("Inputs",
        
        bsActionButton("FMod_runButton", label = c(icon("refresh"),"RUN"), style="primary"),
        
        p(""),
           
        fixedPage(                       
                               
          fixedRow(
            column(width = pageWidth,   
            
              fixedRow(  
                column(width = sectionWidth,   
                  wellPanel(         
                    h4("Simulation design"),
                    fluidRow(
                      column(width = sectionWidth,
                        numericInput("FMod_Tmax", c("Time",getIcon("Info_Tmax")), 100, min = 1, step = 1)
                      ),
                      column(width = sectionWidth,
                        numericInput("FMod_NP",c("Replicates", getIcon("Info_NP")), 1, min = 1, step = 1)
                      )
                    )
                  )   
                ),
                column(width = sectionWidth,
                  wellPanel(         
                    h4("Population design"),
                    fluidRow(
                      column(width = sectionWidth,
                             numericInput("FMod_NI", c("Individual(s)",getIcon("Info_NI")), 5, min = 1, step = 1)
                      ),
                      column(width = sectionWidth,
                             selectInput("FMod_NT", c("Trait(s)",getIcon("Info_NT")),c("1" = 1,"2" = 2)) 
                      )
                    )
                  ) 
                )
              ), # fixedRow
              
              fixedRow(  
                column(width = pageWidth,   
                   wellPanel(         
                     h4("Environment design"),
                     h5(c("Known environment", getIcon("Info"))),
                     fluidRow(
                       column(width = sectionWidth,
                        wellPanel( class="disable",     
                          checkboxInput("FMod_X1_state", "Add environment X1", value = FALSE),
                          conditionalPanel(
                            condition = "input.FMod_X1_state == 1",                              
                            fluidRow(  
                              column(width = sectionWidth,
                                numericInput("FMod_X1_ran_V", c("Variance", getIcon("Info")), 0.1, step=0.01),
                                checkboxInput("FMod_X1_shared", c("Shared environment", getIcon("Info")), value = TRUE)
                              ),
                              column(width = sectionWidth,
                                checkboxInput("FMod_X1_ran_decay", c("Add decay rate", getIcon("Info")), value = FALSE),                              
                                conditionalPanel(
                                  condition = "input.FMod_X1_ran_decay == 1",
                                  numericInput("FMod_X1_ran_alpha", c("Alpha", getIcon("Info")), 0.1, step=0.01)
                                )
                              )
                            )
                          )  
                        )
                       ),
                       column(width = sectionWidth,
                        wellPanel( class="disable",   
                          checkboxInput("FMod_X2_state", "Add environment X2", value = FALSE),
                          conditionalPanel(
                            condition = "input.FMod_X2_state == 1",  
                            fluidRow( 
                              column(width = sectionWidth,
                                numericInput("FMod_X2_ran_V", c("Variance", getIcon("Info")), 0.1, step=0.01),
                                checkboxInput("FMod_X2_shared", c("Shared environment", getIcon("Info")), value = TRUE)
                              ),
                              column(width = sectionWidth,  
                                checkboxInput("FMod_X2_ran_decay", c("Add decay rate", getIcon("Info")), value = FALSE),                              
                                conditionalPanel(
                                  condition = "input.FMod_X2_ran_decay == 1",
                                  numericInput("FMod_X2_ran_alpha", c("Alpha", getIcon("Info")), 0.1, step=0.01)
                                )
                              )
                            )
                            
                          )  
                        )
                       )
                     ), # End Fluidrow (Known environment)
                     
                     conditionalPanel( class="disable", 
                       condition = "input.FMod_X1_state == 1 & input.FMod_X2_state == 1",
                       checkboxInput("FMod_X_Interaction", c("Add interaction between X1 and X2", getIcon("Info")), value = FALSE)                        
                     ),
                     
                     h5("Unknown environment"),
                     fluidRow(                           
                       column(width = sectionWidth,
                          wellPanel( class="disable",   
                            checkboxInput("FMod_EG_state", c("Add general environment (Shared)", getIcon("Info")), value = FALSE),
                            conditionalPanel(
                              condition = "input.FMod_EG_state == 1", 
                              fluidRow( 
                                column(width = sectionWidth,
                                  numericInput("FMod_EG_ran_V", c("Variance", getIcon("Info")), 0.1, step=0.01)
                                ),
                                column(width = sectionWidth,
                                  checkboxInput("FMod_EG_ran_decay", c("with decay rate", getIcon("Info")), value = FALSE),                              
                                  conditionalPanel(
                                    condition = "input.FMod_EG_ran_decay == 1",
                                    numericInput("FMod_EG_ran_alpha", c("Alpha", getIcon("Info")), 0.1, step=0.01)
                                  )
                                )
                              )
                            )  
                          )
                       ),
                       column(width = sectionWidth,
                          wellPanel( class="disable",     
                            checkboxInput("FMod_ES_state", c("Add special environment (Unshared)", getIcon("Info")), value = FALSE),
                            conditionalPanel(
                              condition = "input.FMod_ES_state == 1",
                              fluidRow( 
                                column(width = sectionWidth,
                                  numericInput("FMod_ES_ran_V", c("Variance", getIcon("Info")), 0.1, step=0.0001)
                                ),
                                column(width = sectionWidth,                                
                                  checkboxInput("FMod_ES_ran_decay", c("with decay rate", getIcon("Info")), value = FALSE),                              
                                  conditionalPanel(
                                    condition = "input.FMod_ES_ran_decay == 1",
                                    numericInput("FMod_ES_ran_alpha", c("Alpha", getIcon("Info")), 0.1, step=0.0001)
                                  )
                                )
                              )
                            )  
                          )
                       )
                     ) # End Fluidrow (UnKnown environment) 
                   ) # End wellPanel  
                )
              ), # fluidRow
              
              fixedRow(  
                column(width = pageWidth, 
                  wellPanel(
                    h4("Model equation"),
                    uiOutput('FMod_myEquation1'),
                    uiOutput('FMod_myEquation2')
                  ),  
                  wellPanel( 
                    h4("Individual phenotypes design"),                          
                    uiOutput("FMod_B_UI"),                              
                    uiOutput("FMod_Vind_UI"),                                                                                      
                    numericInput("FMod_Vme", c("Measurement error Variance", getIcon("Info_Vm")), 0.01, min = 0, max = 1, step=0.01)
                  )
                )
              ), 
              
              fixedRow(
                column(width = pageWidth,         
                   wellPanel(
                     
                     h4("Sampling design"),
                     
                     fluidRow(
                       column(width = pageWidth/3,                         
                         numericInput("FMod_NR", c("Records",getIcon("Info_NR")), 20, min = 1, max = 100, step = 1),
                         numericInput("FMod_Vit", c("Among-individual variance in timing of sampling",getIcon("Info_Vit")), 0, min = 0, max = 0.95, step=0.01)
                       ),
                       column(width = pageWidth/3,                     
                         h6("Number of records:"),
                         checkboxInput("FMod_Drec_Ind", c("Same among individuals", getIcon("Info_Drec_Ind")), value = TRUE),
                         checkboxInput("FMod_Drec_Trait", c("Same among traits within individuals",getIcon("Info_Drec_Trait")), value = TRUE)                         
                       ),
                       column(width = pageWidth/3, 
                          h6("Sampling time:"),
                          checkboxInput("FMod_Dtime_Ind", c("Same among individuals", getIcon("Info_Dtime_Ind")), value = TRUE),
                          checkboxInput("FMod_Dtime_Trait", c("Same among traits within individuals",getIcon("Info_Dtime_Trait")), value = TRUE)
                       )
                     )
                   )
                )
              )

            ) # End column 12                               
          )
        
        )
    ),# End tabPanel Inputs
    
      # Outputs panel
    tabPanel("Outputs",
                          
      bsActionButton("FMod_rerunButton", label = c(icon("refresh"),"re-RUN")),
      #              downloadButton("Sim_downloadButton", label = "Download"),
      p(),
      bsProgressBar("FMod_PB", value = 0, visible = FALSE, animate = TRUE),     
      p(),
      navlistPanel(   
        fluid  = FALSE,
        widths = c(2,9),
#         tabPanel("Table", tableOutput("FMod_result")), # END Table 
        tabPanel("Environments", plotOutput("FMod_PlotEnvironment", height = "1400px")), # END Environements 
        tabPanel("Individual Phenotypes", plotOutput("FMod_PlotPhenotype", height = "700px")), # END Individual phenotypes
        tabPanel("Sampling Time", plotOutput("FMod_PlotSamples", height = "350px")) # END Environements 
      )
             
    ) # END tabpanel Outputs
    
  ), # End SimTabsetPanel

  HTML("</div>")

  )# End fixedPage

  ) # End return
  
}