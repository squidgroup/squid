# My Shiny User Interface #

#### Include #### 
source("./source/UIsource.R", local=TRUE)



shinyUI(
  fluidPage(

  #### Header  ####
  #### CSS and Javascipt file ####  
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
    tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ]}});", type='text/x-mathjax-config')
  ),
  
  #### For debugging ####
  # verbatimTextOutput("debug"),
  
  
  # Navigation upper Bar
  navbarPage( 
    #### Title ####
    title = "SQuID", selected = "Home",

    #### Home ####
    tabPanel("Home", icon=icon("home", "fa-fw"),
    fixedPage(tags$div(class="myPage myTutorial", # Background container
             
    tabsetPanel(id = "Home_TabsetPanel",
                
      #### Portal #### 
      tabPanel("Portal", # Title
        source("./source/pages/portal/UIportal.R",local=TRUE)[["value"]]
      ), # End tabPanel Portal
      
      #### Background #### 
      tabPanel("Background", # Title
          wellPanel( 
            h4(portal_txt$background_title),
            p(HTML(portal_txt$background_content_1)),
            p(HTML(portal_txt$background_content_2))),
          wellPanel( 
            h4(portal_txt$parag1_title),
            p(HTML(portal_txt$parag1_contents))
          )
      )# End tabPanel Background
          
    )))), # End tabPanel Home
    
    #### Modules page ####
    source("./source/pages/modules/UImodules.R",local=TRUE)[["value"]],

    #### Simulate Full Model ####
    navbarMenu("Simulation",
               
      #### Full Model (Step by step) ####
      tabPanel("Full model (Step by step)", # title
               icon=icon("coffee", "fa-fw"), # Icon
               # Simulation container 
               fixedPage(tags$div(class="myPage", source("./source/pages/fullModelSbyS/UIfullModelSbyS.R",local=TRUE)[["value"]]))
      ), # End tabPanel Simulation 
  
      #### Full model (express) ####
      tabPanel("Full model (express)", # title
               icon=icon("fighter-jet", "fa-fw"), # Icon
               # Simulation container 
               tags$div(class="myPage",source("./source/pages/fullModel/UIfullModel.R",local=TRUE)[["value"]])
      ) # End tabPanel Full Model 
      
    ), # End navBarMenu Simulation
    
    #### R code ####
    tabPanel("squidR",
    				 fixedPage(wellPanel(shiny::includeMarkdown("./source/pages/squidR/squidR.md")))
    ), # End tabPanel People
    
    #### About us ####
    tabPanel("About us", icon=icon("users", "fa-users"),
       tags$div(class="myPage myTutorial",
        fixedPage(
          wellPanel( 
            h4("Contact us"),
            p("Visit us on ", a("Github.", href="https://github.com/hallegue/squid", target="_blank")),
            p("Report a ", a("bug.", href="https://github.com/hallegue/squid/issues", target="_blank"))
          ), 
          wellPanel( 
            h4(portal_txt$parag2_title),
            p(HTML(portal_txt$parag2_contents))
          ),
          wellPanel(
            h4(portal_txt$parag4_title),
            p(HTML('<img src="pictures/group_pic.jpg" alt="SQuID">')),
            p(HTML(portal_txt$parag4_contents))
          )
        )
       )
    ), # End tabPanel About us

    #### Footer ####
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
