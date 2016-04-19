#' @export
#' 
squidApp <- function(launch.browser=TRUE, ...) {
  
  f   <- factor(c("a", "b"))
  res <- lme4::dummy(f)
  cat.var <- rep(1:2, 2)
  res <- dim(arm::contr.bayes.unordered(cat.var))
  
  appDir <- system.file("shiny-squid", package = "squid")
  
  if (appDir == "") stop("Could not find squid application directory. Try re-installing `squid` package.", call. = FALSE)
  
  shiny::runApp(appDir=appDir, launch.browser=launch.browser, display.mode="normal", ...)
}