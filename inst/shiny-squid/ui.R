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
    
    # Background page
    tabPanel("Background", # Title
             icon=icon("folder-open", "fa-fw"),
             # Background container
             tags$div(class="myPage myTutorial",
                fixedPage(
                  wellPanel( 
                    h4(portal_txt$background_title),
                    p(HTML(portal_txt$background_content_1)),
                    p(HTML(portal_txt$background_content_2)),
                    h4(portal_txt$parag1_title),
                    p(HTML(portal_txt$parag1_contents))
                  )
                )
             )
             
    ), # End tabPanel Background
    
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

    # Contact us page
    tabPanel("R code",
    				 fixedPage(wellPanel(shiny::includeMarkdown("./source/pages/Rcode/Rcode.md")))
    ), # End tabPanel People
    
    # About us page
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
    ), # End tabPanel About us

    # Contact us page
    tabPanel("Contact us", icon=icon("envelope", "fa-users"),
    				 
             tags$div(class="myPage myTutorial",
                fixedPage(
                  wellPanel( 
                    h2("Contact us"),
                    p("Visit us on ", a("Github.", href="https://github.com/hallegue/squid", target="_blank")),
                    p("Report a ", a("bug.", href="https://github.com/hallegue/squid/issues", target="_blank"))
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
