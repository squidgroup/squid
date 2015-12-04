#' Run squid function
#'
#' @return NULL
#' @export
runSQUIDfct <- function() {
  
  appDir <- system.file("shiny-squid", "myapp", "include", "myInclude", "fullModel", "functions",
                        package = "SQUID")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `squid`.", call. = FALSE)
  }
  
  getwd()
   
}