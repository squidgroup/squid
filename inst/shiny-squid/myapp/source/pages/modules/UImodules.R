fixedPage( HTML("<div>"),                                 
   
   navlistPanel( 
     
     id       = "modulesNavList",
     selected = Module_titles$mod1,
     well     = TRUE,  
     fluid    = FALSE,
     widths   = c(2,9),
            
     "Modules",
     
   ####### Module 1 #######################################################################################
     tabPanel(Module_titles$mod1, 
              wellPanel( 
                
                # Title
                h3(HTML(module1_txt$title)),
                p(HTML(module1_txt$goal)), 
                
                tabsetPanel(id = "Module1TabsetPanel", type = "pills", selected = "Step 1",
                            
                            # Step 1 ------------------------------------------------------------
                            tabPanel("Step 1", source("./source/pages/modules/module1/UIMod1Step1.R",local=TRUE)),    
                            
                            # Step 2 ------------------------------------------------------------
                            tabPanel("Step 2", source("./source/pages/modules/module1/UIMod1Step2.R",local=TRUE)),
                            
                            # Step 3 ------------------------------------------------------------
                            tabPanel("Step 3", source("./source/pages/modules/module1/UIMod1Step3.R",local=TRUE)),
                            
                            # Step 4 ------------------------------------------------------------
                            tabPanel("Step 4", source("./source/pages/modules/module1/UIMod1Step4.R",local=TRUE))
                            
                ) # End tabsetPanel
              ) # End Wellpanel
     ), # END Module 1               
     
     # Module 2
     # tabPanel("Module 2",UImodule2()), # END Module 2
     
   ####### Module 3 #######################################################################################
      tabPanel(Module_titles$mod3, 
              
              wellPanel( 
                
                # Title
                h3(HTML(module3_txt$title)),
                
                tabsetPanel(id = "Module3TabsetPanel", type = "pills", selected = "Step 1",
                            
                            # Step 1 ------------------------------------------------------------
                            tabPanel("Step 1", source("./source/pages/modules/module3/UIMod3Step1.R",local=TRUE)),    
                            
                            # Step 2 ------------------------------------------------------------
                            tabPanel("Step 2", source("./source/pages/modules/module3/UIMod3Step2.R",local=TRUE)),
                            
                            # Step 3 ------------------------------------------------------------
                            tabPanel("Step 3", source("./source/pages/modules/module3/UIMod3Step3.R",local=TRUE))
                            
                )
              )
     ), # END Module 3
     
#          # Module 4
#          tabPanel("Module 4",UImodule4()), # END Module 4
#          
#          # Module 5
#          tabPanel("Module 5",UImodule5()), # END Module 5
     
   ####### Module 1 #######################################################################################
     tabPanel(Module_titles$mod6,
              
              wellPanel( 
                
                # Title
                h3(HTML(module6_txt$title)),
                p(HTML(module6_txt$goal)), 
                
                tabsetPanel(id = "Module6TabsetPanel", type = "pills", selected = "Step 1",
                            
                            # Step 1 ------------------------------------------------------------
                            tabPanel("Step 1", source("./source/pages/modules/module6/UIMod6Step1.R",local=TRUE)),   
                            
                            # Step 2 ------------------------------------------------------------
                            tabPanel("Step 2", source("./source/pages/modules/module6/UIMod6Step2.R",local=TRUE)),
                            
                            # Step 3 ------------------------------------------------------------
                            tabPanel("Step 3",source("./source/pages/modules/module6/UIMod6Step3.R",local=TRUE))
                ) # End tabsetPanel
              ) # End Wellpanel   
     ) # END Module 6
     



#          # Module 7
#          tabPanel("Module 7",UImodule7()), # END Module 7
#          
#          # Module 8
#          tabPanel("Module 8",UImodule8()), # END Module 8
#          
#          # Module 9
#          tabPanel("Module 9",UImodule9()) # END Module 9
     
#          # Module 10
#          tabPanel("Module 10",module10()) # END Module 10
     
     
  ),
  HTML("</div>") # END div -> id=portal
)