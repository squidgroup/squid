# create an icon into input label
actionButton2 <- function (inputId, label, icon = NULL, ...) 
{
  tags$button(id = inputId, type = "button", class = "btn action-button btn-primary", 
              list(icon, label))
}