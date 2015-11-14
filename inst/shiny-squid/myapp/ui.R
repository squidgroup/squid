
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# 

#################################################################
## Include packages and source functions ########################

source("include/include.R")

#################################################################

#################################################################
#################################################################
########### My Shiny User Interface Main Page ###################
#################################################################
#################################################################

shinyUI( fluidPage(
 
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
    # tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
#     tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript'),
    tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ]}});", type='text/x-mathjax-config'),
    tags$script(type="text/javascript", src = "js/googleAnalytics.js")
  ),
  
  # Navigation upper Bar
  navbarPage( 

#     collapsable = TRUE,
    
    # Application title
    title = "SQUID",
    
#     tabPanel("Home", # Title
#       icon=icon("home", "fa-fw"), # Icon
#       # Home container    
#       tags$div(class="myPage",home())
#     ), # END tabpanel Home        

    # Portal page
    tabPanel("Portal", # Title
             icon=icon("tasks", "fa-fw"), # Icon
             # Portal container
             tags$div(class="myPage myTutorial", portal())
             
    ), # End tabPanel Portal
    
    # Modules page
    tabPanel("Modules", # Title
             icon=icon("tasks", "fa-fw"), # Icon
             # Portal container
             tags$div(class="myPage myTutorial", UImodules())
             
    ), # End tabPanel Portal

    # Simulation page
    tabPanel("Full model (Step by step)", # title
             icon=icon("coffee", "fa-fw"), # Icon
             # Simulation container 
             tags$div(class="myPage",UIfullModelSbyS())
             
    ), # End tabPanel Simulation 

    # Full model page
    tabPanel("Full model (express)", # title
             icon=icon("fighter-jet", "fa-fw"), # Icon
             # Simulation container 
             tags$div(class="myPage",UIfullModel())
             
    ), # End tabPanel Full Model 

      
    # Footer
    tags$div(class="footer",
             fixedPage(div(class="line"),
             # Boolean showing when a simulation is running
             conditionalPanel(
               condition = "0",         
               checkboxInput("isRunning", "", FALSE)
             ), 
             " "
            ))

#     # People page
#     tabPanel("Peolple", icon=icon("users", "fa-users"),             
#              h3("People page"),
#              tags$ul(tags$li("Information about the persons involved in this project"))), # End tabPanel People
#     
#     # Contact page
#     tabPanel("Contact", icon=icon("envelope", "fa-fw"),
#              h3("Contact page"),
#              tags$ul(tags$li("Info and help contact"))) # End tabPanel Contact
  ) # END navbarPage
)
)
