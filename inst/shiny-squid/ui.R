#################################################################
#################################################################
################ My Shiny User Interface  #######################
#################################################################
#################################################################

## Include packages and source functions ########################
source("./source/UIsource.R", local=TRUE)

shinyUI(
  fluidPage(

  # Header
  # add custom CSS and Javascipt file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/jquery.qtip.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/stylesheet.css"),
    tags$script(type="text/javascript", src = "js/jquery.qtip.js"),
    tags$script(type="text/javascript", src = "js/myScript.js"),
    tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", 
                      function(message) { 
                        console.log(message)
                        eval(message.code); 
                      });')),
    tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ]}});", type='text/x-mathjax-config'),
    tags$script(type="text/javascript", src = "js/googleAnalytics.js")
  ),
  
  # Navigation upper Bar
  navbarPage( 
    # Application title
    title = "SQuID",

    # Portal page
    tabPanel("Portal", # Title
             icon=icon("home", "fa-fw"),
             # Portal container
             tags$div(class="myPage myTutorial", 
                      source("./source/pages/portal/UIportal.R",local=TRUE)[["value"]]
                      )
             
    ), # End tabPanel Portal
    
    # Modules page
    tabPanel("Modules", # Title
             icon=icon("tasks", "fa-fw"), # Icon
             # Portal container
             tags$div(class="myPage myTutorial", 
                      # UImodules()
                      source("./source/pages/modules/UImodules.R",local=TRUE)[["value"]]
                      )
    ), # End tabPanel Portal

    # Simulation page
    tabPanel("Full model (Step by step)", # title
             icon=icon("coffee", "fa-fw"), # Icon
             # Simulation container 
             tags$div(class="myPage",source("./source/pages/fullModelSbyS/UIfullModelSbyS.R",local=TRUE)[["value"]])
    ), # End tabPanel Simulation 

    # Full model page
    tabPanel("Full model (express)", # title
             icon=icon("fighter-jet", "fa-fw"), # Icon
             # Simulation container 
             tags$div(class="myPage",source("./source/pages/fullModel/UIfullModel.R",local=TRUE)[["value"]])
    ), # End tabPanel Full Model 

    # People page
    tabPanel("About us", icon=icon("users", "fa-users"),
       tags$div(class="myPage myTutorial",
        fixedPage(
          wellPanel( 
            h4(portal_txt$parag2_title),
            p(HTML(portal_txt$parag2_contents)),
            h4(portal_txt$parag4_title),
            fluidRow(
              column(6, HTML('<img src="pictures/group_pic.jpg" alt="SQuID">')),
              column(6, p(HTML(portal_txt$parag4_contents)))
            )
          )
        )
       )
    ), # End tabPanel People

    # Footer
    tags$div(class="footer",
     fixedPage(div(class="line"),
       # Boolean showing when a simulation is running
       conditionalPanel(
         condition = "0",         
         checkboxInput("isRunning", "", FALSE)
       ), 
       " "
     )
    )
  ) # END navbarPage
)
)
