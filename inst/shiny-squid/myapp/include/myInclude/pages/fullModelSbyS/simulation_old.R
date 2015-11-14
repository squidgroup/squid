simulation_old <- function(){
  
  return(
    
    fixedPage( HTML("<div id='Simulation'>"), 
               h2("Full simulation model"),
               
    # Inputs and Outputs tabset panel
    tabsetPanel(id = "SimTabsetPanel", type = "tabs",
                
      # Inputs panel
      tabPanel("Inputs", 
                 
                           
         wellPanel(
          h4("Model equation"),
          uiOutput('Sim_myEquation1'),
          uiOutput('Sim_myEquation2')
         ), 
         
         bsActionButton("runButton", label = c(icon("refresh"),"RUN"), style="primary"),         
         p(),    
         
         # Inputspage     
         fixedPage( HTML("<div id='Inputspage'>"),                                                               
                                      
            navlistPanel(  
              
              well   = TRUE,  
              fluid  = FALSE,
              widths = c(3,8),
              
              "Simulation",
              
              # Populations Design
              tabPanel("Simulation design",
                       
                wellPanel(
                  h4("Simulation design"),                                                
                  numericInput("Sim_Tmax", c("Time",getIcon("Info_Tmax")), 100, min = 1, step = 1),
                  numericInput("Sim_NP",c("Replicates", getIcon("Info_NP")), 1, min = 1, step = 1)
                )                       
              ), # END Simulation design              

              "-----",
              "Environment",
              
              # Traits Design
              tabPanel("Environment design",    
                                              
                wellPanel(
                  h4("Environment design"),
                  
                  fluidRow(
                    h5(c("Known environment", getIcon("Info"))),
                    column(6, class="noMargin",
                           wellPanel(     
                             checkboxInput("Sim_X1_state", h5("Add environment X1"), value = FALSE),
                             conditionalPanel(
                               condition = "input.Sim_X1_state == 1",                              
                               checkboxInput("Sim_X1_shared", c("Shared environment",getIcon("Info")), value = TRUE),
                               numericInput("Sim_X1_ran_V", c("Variance",getIcon("Info")), 0.1, step=0.01),
                               checkboxInput("Sim_X1_ran_decay", c("Add decay rate",getIcon("Info")), value = FALSE),                              
                               conditionalPanel(
                                 condition = "input.Sim_X1_ran_decay == 1",
                                 numericInput("Sim_X1_ran_alpha", c("Alpha",getIcon("Info")), 0.1, step=0.01)
                               )                              
                             )  
                           )
                    ),
                    column(6,
                           wellPanel(    
                             checkboxInput("Sim_X2_state", h5("Add environment X2"), value = FALSE),
                             conditionalPanel(
                               condition = "input.Sim_X2_state == 1",                              
                               checkboxInput("Sim_X2_shared", c("Shared environment",getIcon("Info")), value = TRUE),
                               numericInput("Sim_X2_ran_V", c("Variance",getIcon("Info")), 0.1, step=0.01),
                               checkboxInput("Sim_X2_ran_decay", c("Add decay rate",getIcon("Info")), value = FALSE),                              
                               conditionalPanel(
                                 condition = "input.Sim_X2_ran_decay == 1",
                                 numericInput("Sim_X2_ran_alpha", c("Alpha",getIcon("Info")), 0.1, step=0.01)
                               )                              
                             )  
                           )
                    )
                  ), # End Fluidrow (Known environment)
                  conditionalPanel(
                    condition = "input.Sim_X1_state == 1 & input.Sim_X2_state == 1",
                    checkboxInput("Sim_X_Interaction", c("Add interaction between X1 and X2",getIcon("Info")), value = FALSE)                        
                  ),
                  
                  fluidRow(
                    h5(c("Unknown environment",getIcon("Info"))),
                    column(6, class="noMargin",
                           wellPanel(     
                             checkboxInput("Sim_EG_state", h5(c("Add general environment (Shared)", getIcon("Info"))), value = FALSE),
                             conditionalPanel(
                               condition = "input.Sim_EG_state == 1",                                                                 
                               numericInput("Sim_EG_ran_V", c("Variance",getIcon("Info")), 0.1, step=0.01),
                               checkboxInput("Sim_EG_ran_decay", c("Add decay rate",getIcon("Info")), value = FALSE),                              
                               conditionalPanel(
                                 condition = "input.Sim_EG_ran_decay == 1",
                                 numericInput("Sim_EG_ran_alpha", c("Alpha",getIcon("Info")), 0.1, step=0.01)
                               )                              
                             )  
                           )
                    ),
                    column(6,
                           wellPanel(    
                             checkboxInput("Sim_ES_state", h5(c("Add special environment (Unshared)",getIcon("Info"))), value = FALSE),
                             conditionalPanel(
                               condition = "input.Sim_ES_state == 1",                              
                               numericInput("Sim_ES_ran_V", c("Variance",getIcon("Info")), 0.1, step=0.0001),
                               checkboxInput("Sim_ES_ran_decay", c("Add decay rate",getIcon("Info")), value = FALSE),                              
                               conditionalPanel(
                                 condition = "input.Sim_ES_ran_decay == 1",
                                 numericInput("Sim_ES_ran_alpha", c("Alpha",getIcon("Info")), 0.1, step=0.0001)
                               )                              
                             )  
                           )
                    )
                  ) # End Fluidrow (Known environment)                  
                )
              ),
              
              
              "-----",
              "Populations",
              
              # Populations Design
              tabPanel("Population design",                       
                wellPanel(
                  h4("Population design"),                                                
                  numericInput("Sim_NI", c("Individual(s)",getIcon("Info_NI")), 5, min = 1, step = 1),
                  selectInput("Sim_NT", c("Trait(s)",getIcon("Info_NT")),c("1" = 1,"2" = 2)),
                  div(class="space")
                )
              ), # END Populations design 
              
              # Traits Design
              tabPanel("Individual phenotypes design",
                wellPanel(
                  h4("Individual phenotypes design"),                          
#                   uiOutput("Sim_B_UI"),          
  #                         matrixInputB("Sim_B", c("Population mean values", getIcon("Info_B")), B, inputOn),
                                          
#                   uiOutput("Sim_Vind_UI"),                                                                        
  #                         matrixInputVind("Sim_Vind", c("Individuals (Co)Variance matrix", getIcon("Info_Vind")), Vind, inputOn2),    
                  
                  numericInput("Sim_Vme", c("Measurement error Variance", getIcon("Info_Vm")), 0.01, min = 0, max = 1, step=0.01)                  
                )
              ), # END Traits design                           
              
              "-----",
              "Sampling",
              
              # Sampling Design
              tabPanel("Sampling design",        
                wellPanel(
                  h4("Sampling design"),
                  numericInput("Sim_NR", c("Records",getIcon("Info_NR")), 20, min = 1, max = 100, step = 1),                         
                  numericInput("Sim_Vit", c("Among-individual variance in timing of sampling",getIcon("Info_Vit")), 0, min = 0, max = 0.95, step=0.01),                      
  #                         sliderInput("Sim_Time_sampling", c("Sampling interval (%)",getIcon("Info_Time_sampling")),min = 0, max = 100, value = c(0,100)),
  #                         verbatimTextOutput("Sim_Time_sampling_text"),
                  h6("Number of records:"),
                  checkboxInput("Sim_Drec_Ind", c("Same among individuals", getIcon("Info_Drec_Ind")), value = TRUE),
                  checkboxInput("Sim_Drec_Trait", c("Same among traits within individuals",getIcon("Info_Drec_Trait")), value = TRUE),
                  h6("Sampling time:"),
                  checkboxInput("Sim_Dtime_Ind", c("Same among individuals", getIcon("Info_Dtime_Ind")), value = TRUE),
                  checkboxInput("Sim_Dtime_Trait", c("Same among traits within individuals",getIcon("Info_Dtime_Trait")), value = TRUE)
                  
                )
             )            
            ), # END navlistPanel              
            
            HTML("</div>") # END div -> id=Inputspage
                    
         ) # END Inputspage
      ),# End tabPanel Inputs
      
      # Outputs panel
      tabPanel("Outputs",
               
               actionButton("rerunButton", "re-RUN", icon = NULL),
               
               #              downloadButton("Sim_downloadButton", label = "Download"),
               p(),
               bsProgressBar("Sim_PB", value = 0, visible = FALSE, animate = TRUE),     
               p(),
               navlistPanel( 
                 #         well   = TRUE,  
                 fluid  = FALSE,
                 widths = c(2,9),
#                  tabPanel("Table", tableOutput("Sim_result")), # END Table 
                 tabPanel("Environments", plotOutput("Sim_PlotEnvironment", height = "1400px")), # END Environements 
                 tabPanel("Individual Phenotypes", plotOutput("Sim_PlotPhenotype", height = "700px")), # END Individual phenotypes
                 tabPanel("Sampling Time", plotOutput("Sim_PlotSamples", height = "350px")) # END Environements 
               )
               
      ) # END tabpanel Outputs
                
    ), # END SimTabsetPanel
    
    HTML("</div>")

    )# End fluidPage
    
  )
  
}
