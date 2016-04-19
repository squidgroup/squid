# @title Plot an environmental effect
#
# @description          \code{plotEnvironment} generates and plots the time serie of an environmental effect values.
#
# @param input          list providing the environmental effect parameters (see details).
# @param module         character of the module name. This parameter is only used by SQUID app.
# @param envName        character of the name of the environment. This parameter is only used by SQUID app.
#
# @return               a \href{https://cran.r-project.org/web/packages/ggplot2/index.html}{ggplot2} plot
# 
# @details
# 
# The argument \code{input} is a list that contains all the parameters needed to generate the desired environment effect. All possible parameters are presented below including their default value if not defined.
#
# \itemize{
#    \item{sto_state: }{(default FALSE) logical value. If TRUE a stochastic environmental effect will be added to the total environmental effect.}
#    \item{sto_V: }{(default 1) }
#    \item{sto_autocor_state: }{(default FALSE) }
#    \item{sto_corr: }{(default 0) }
#    \item{lin_intercept: }{(default FALSE) }
#    \item{lin_slope: }{(default FALSE) }
#    
# }
# 
# @examples
# input <- list()
#
# # Stochastic environmental effect
# input$sto_state  <- TRUE
# 
# # Linear environmental effect
# input$lin_state  <- TRUE
#
# plotEnvironment(input)
# 
#
#
plotEnvironment <- function(input, module=NULL, envName=NULL){
  
  x<-colour<-NULL
  
  sep         <- ifelse(is.null(module), "", "_")
  input$state <- TRUE
  myX         <- createEnvironment(input, module, envName, sep)
  
  inputNames <- list(
    "NI"             = paste(module, "NI"  , sep = sep),
    "Tmax"           = paste(module, "Tmax", sep = sep)  
  )
  
  # Time 
  Time <- list(
    "Tmin" = 1, # Start time
    "Tmax" = ifelse(inputNames$Tmax %in% names(input),input[[inputNames$Tmax]],2), # End time
    "TS"   = 1  # Time step value
  )
  
  N <- list(
    "NP"  = 1,   # Number of populations
    "NI"  = ifelse(inputNames$NI %in% names(input),input[[inputNames$NI]],1), # Number of individuals (between 2 and inf)
    "NT"  = 1,   # Number of traits
    "NS"  = (Time$Tmax - Time$Tmin + 1)/Time$TS # Number of step of time
  )
  
  envData  <- getEnvironment(myX, N, TRUE)
  myData   <- data.frame("envData"  = envData, 
                         "x"        = rep(1:N$NS, N$NI),
                         "colour"   = as.factor(rep(1:N$NI, each = N$NS)))
  
  myPlot   <- ggplot2::ggplot(myData, ggplot2::aes(y=envData, x=x, colour=colour)) +
                              ggplot2::geom_point() +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment") + 
                              ggplot2::theme(legend.position="none")
 
  return(myPlot)
}