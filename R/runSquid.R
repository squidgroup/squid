#' Title
#'
#' @return
#' @export
#'
#' @examples
runSquid <- function() {
  appDir <- system.file("shiny-squid", "myapp", package = "SQUID")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `squid`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}