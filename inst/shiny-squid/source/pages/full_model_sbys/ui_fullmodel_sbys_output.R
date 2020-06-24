span(
  p(HTML(fullmodelTxt$output_1)),
  p(HTML(fullmodelTxt$output_2)),
  p(HTML(fullmodelTxt$output_3)), 
  p(HTML(fullmodelTxt$output_4)),
  p(HTML(fullmodelTxt$output_5)),
  
  actionButton("FModSbyS_rerunButton", label = FullModel_VAR$Run$label, icon= FullModel_VAR$Run$icon, class="runButton"),
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
             source("./source/pages/full_model_sbys/ui_fullmodel_equation.R",local=TRUE)[["value"]],
             uiOutput("FModSbyS_Data_Description_Table")
    ) # END Environements 
  )
)