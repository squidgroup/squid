wellPanel( 
  HTML("<div id='FullModelStepByStep'>"), 
    h2("Full model simulation (a step by step approach)"),
    
    p(HTML(fullmodelTxt$fullModelSbyS_intro_1)),
    p(HTML(fullmodelTxt$fullModelSbyS_intro_2)),
    
    # Inputs and Outputs tabset panel
    tabsetPanel(id = "FModSbyS_TabsetPanel", type = "pills",
                
      # model description panel
      tabPanel("Description", 
        # Description page 
        p(HTML(fullmodelTxt$ModelDesciption_intro_1)),       
        tabsetPanel(id = "FModSbyS_Description_TabsetPanel", type = "tabs", selected="Simulation model",
          
          # Full model description
          tabPanel("Simulation model", 
            source("./source/pages/full_model_sbys/ui_fullmodel_sbys_description_full_model.R",local=TRUE)[["value"]]         
          ),
          # Model Summary
          tabPanel("Model summary", 
                   source("./source/pages/full_model_sbys/ui_fullmodel_sbys_description_summary.R",local=TRUE)[["value"]]
          ),
          # Bivariate Model Summary
          tabPanel("Bivariate model",
                   source("./source/pages/full_model_sbys/ui_fullmodel_sbys_description_bivariate_summary.R",local=TRUE)[["value"]]
          ),
          # Sampling design description
          tabPanel("Sampling design", 
            source("./source/pages/full_model_sbys/ui_fullmodel_sbys_description_sampling_design.R",local=TRUE)[["value"]]  
          )         
        )
      ),
      
      # Inputs panel
      tabPanel("Inputs",
        source("./source/pages/full_model_sbys/ui_fullmodel_sbys_input.R",local=TRUE)[["value"]]
      ),# End tabPanel Inputs
    
      # Outputs panel
      tabPanel("Outputs",
        source("./source/pages/full_model_sbys/ui_fullmodel_sbys_output.R",local=TRUE)[["value"]]
      ) # END tabpanel Outputs
    
    ), # End FModSbyS_TabsetPanel

  HTML("</div>")

)# End fixedPage