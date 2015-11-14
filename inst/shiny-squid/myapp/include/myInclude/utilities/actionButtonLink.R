# create an icon into input label
actionButtonLink <- function (inputId, label, icon = NULL, iconSide, ...) 
{
  if(iconSide == "R"){myList <- list(label, icon)}else{myList <- list(icon, label)} 
  
  tags$button(id = inputId, type = "button", class = "btn action-button btn-primary button-link", 
              myList)
}