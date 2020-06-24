# display R code in console format
displayRCode <- function(RCode){
  return(wellPanel(class="highlight-source-r ", HTML(paste0("<h3>R code:</h3>", RCode))))
}