
home <- function(){
  
  return(
    #Home page
    fluidPage(
     h3(home.txt$tite), 
     p(home.txt$parag1),
     tags$ul(tags$li("Context"), 
             tags$li("Objective"),
             tags$li("How it works"),
             tags$li("Persons involved"),
             tags$li("Etc."))
    )
                 
  )
  
}