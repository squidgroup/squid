
fixedPage( HTML("<div id='FullModelStepByStep'>"), 
  h2("Full model simulation (a step by step approach)"),
  
  p(HTML(fullmodelTxt$fullModelSbyS_intro_1)),
  p(HTML(fullmodelTxt$fullModelSbyS_intro_2)),
  
  # Inputs and Outputs tabset panel
  tabsetPanel(id = "FModSbyS_TabsetPanel", type = "pills",
              
    # model description panel
    tabPanel("Description", 
      source("./source/pages/fullModelSbyS/UImodelDescription.R",local=TRUE)
    ),

    # Inputs panel
    tabPanel("Inputs",
      source("./source/pages/fullModelSbyS/UIfullModelSbySInput.R",local=TRUE)
    ),# End tabPanel Inputs
  
    # Outputs panel
    tabPanel("Outputs",
        
      p(HTML(fullmodelTxt$output_1)),
      p(HTML(fullmodelTxt$output_2)),
      p(HTML(fullmodelTxt$output_3)), 
      p(HTML(fullmodelTxt$output_4)),
      p(HTML(fullmodelTxt$output_5)),
                
      bsButton("FModSbyS_rerunButton", label = FullModel_VAR$ReRun$label, icon= FullModel_VAR$ReRun$icon, class="runButton", style = FullModel_VAR$ReRun$style),       
      downloadButton("FModSbyS_download_sampled", label = FullModel_VAR$download_sampled$label),
      downloadButton("FModSbyS_download_raw", label = FullModel_VAR$download_raw$label),
      runningIndicator(),
      
      p(),
      uiOutput("FModSbyS_rerunButtonError"),
      p(),
      
      p(HTML(fullmodelTxt$output_figure_color)),
      
      navlistPanel(   
        fluid  = FALSE,
        widths = c(2,9),
        #         tabPanel("Table", tableOutput("FModSbyS_result")), # END Table 
        tabPanel("Environments",plotOutput("FModSbyS_plotEnvironment", height = "1050px")), # END Environments 
        tabPanel("Individual Phenotypes", plotOutput("FModSbyS_plotPhenotype", height = "700px")), # END Individual phenotypes
        tabPanel("Sampling Time", plotOutput("FModSbyS_plotSamples", height = "350px")),
        tabPanel("Data file description", 
                 tags$b("Full model equation"),
                 source("./source/pages/fullModelSbyS/UImodelEquation.R",local=TRUE),
                 uiOutput("FModSbyS_Data_Description_Table")
               ) # END Environements 
      )
           
    ) # END tabpanel Outputs
  
  ), # End FModSbyS_TabsetPanel

  HTML("</div>")

)# End fixedPage