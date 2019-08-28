#' @export
#' 
squidApp <- function(...) {
  
  appDir <- system.file("shiny-squid", package = "squid")
  
  if (appDir == "") stop("Could not find squid application directory. Try re-installing `squid` package.", call. = FALSE)
  
  shiny::runApp(appDir=appDir, ...)
}