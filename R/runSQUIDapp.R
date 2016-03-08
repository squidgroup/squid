#' Run SQUID app
#'
#' @return NULL
#' @import dplyr
#' @export
runSQUIDapp <- function() {
  appDir <- system.file("shiny-squid", package = "SQUID")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `squid`.", call. = FALSE)
  }
  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}