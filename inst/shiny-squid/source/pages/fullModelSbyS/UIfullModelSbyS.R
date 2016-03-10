fixedPage( HTML("<div id='FullModelStepByStep'>"), 
  h2("Full model simulation (a step by step approach)"),
  
  p(HTML(fullmodelTxt$fullModelSbyS_intro_1)),
  p(HTML(fullmodelTxt$fullModelSbyS_intro_2)),
  
  # Inputs and Outputs tabset panel
  tabsetPanel(id = "FModSbyS_TabsetPanel", type = "pills",
              
    # model description panel
    tabPanel("Description", 
      source("./source/pages/fullModelSbyS/UIfullModelSbySDescription.R",local=TRUE)[["value"]]
    ),

    # Inputs panel
    tabPanel("Inputs",
      source("./source/pages/fullModelSbyS/UIfullModelSbySInput.R",local=TRUE)[["value"]]
    ),# End tabPanel Inputs
  
    # Outputs panel
    tabPanel("Outputs",
      source("./source/pages/fullModelSbyS/UIfullModelSbySOutput.R",local=TRUE)[["value"]]
    ) # END tabpanel Outputs
  
  ), # End FModSbyS_TabsetPanel

  HTML("</div>")

)# End fixedPage