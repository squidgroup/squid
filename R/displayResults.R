# multiplot: Multiple plot function
#
# Args:
#   ...         ggplot objects
#   plotlist:   list of ggplot objects
#   cols:       Number of columns in layout
#   layout:     A matrix specifying the layout. If present, 'cols' is ignored.
#
# Returns:
#   list of an environment object.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots <- length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}



# plotEnvironment: plot an environmental effect
#
# Args:
#    input     list providing the environmental effect parameters (see details).
#    module    character of the module name. This parameter is only used by SQUID app.
#    envName   character of the name of the environment. This parameter is only used by SQUID app.
#
# Returns:
#   a ggplot plot.

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

# displayResults: create ggplot plots showing the results of the simulation.
#
# Args:
#   N:            internal list of simulation variables (related to simulation design). 
#   Time:         internal list of simulation variables (related to simulation timing).
#   full_Data:    data.frame; the full data generated.
#   sampled_Data: data.frame; the sampled data 
#
# Returns:
#   list of ggplot plots

displayResults <- function(N, Time, full_Data, sampled_Data){ 
    
  Trait <- Replicate <- Individual <- Phenotype <- Individual_Trait <- X1 <- X2 <- X1X2 <- NULL
  
  full_Data$Individual    <- as.numeric(as.character(full_Data$Individual))
  sampled_Data$Individual <- as.numeric(as.character(sampled_Data$Individual))
  
  # subset data
  full_Data2    <- copy(data.table::as.data.table(full_Data))[   Trait == 1 & Replicate == 1 & Individual <= 20]
  sampled_Data2 <- copy(data.table::as.data.table(sampled_Data))[Trait == 1 & Replicate == 1 & Individual <= 20]
  
  # Display phenotype
  plot_TotPhen <-  ggplot2::ggplot(data = full_Data2, ggplot2::aes(x     = Time, 
                                                                y     = Phenotype, 
                                                                color = as.factor(Individual), 
                                                                group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Total individual phenotypes") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Phenotype") +
                                    ggplot2::theme(legend.position = "none")
  

  #Display sampling phenotype
  plot_SampPhen <-  ggplot2::ggplot(data = sampled_Data2, ggplot2::aes(x     = Time, 
                                                                    y     = Phenotype, 
                                                                    color = as.factor(Individual), 
                                                                    group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Sampled individual phenotypes") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Phenotype") +
                                    ggplot2::theme(legend.position = "none")
  
  # Display sampling time
  plot_SampTime <-  ggplot2::ggplot(data = sampled_Data2, ggplot2::aes(x     = Time, 
                                                                    y     = Individual_Trait, 
                                                                    color = as.factor(Individual), 
                                                                    group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Sampling time per individual") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Individuals") +
                                    ggplot2::theme(legend.position = "none")
  
  # Display environment X1
  plot_X1 <-  ggplot2::ggplot(data = full_Data2, ggplot2::aes(x     = Time, 
                                                           y     = X1, 
                                                           color = as.factor(Individual), 
                                                           group = Individual)) +
                              ggplot2::geom_point() +
                              ggplot2::geom_line() +
                              ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                              ggplot2::ggtitle("Environment over time") +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment X1") +
                              ggplot2::theme(legend.position = "none")
  
  # Display environment X2
  plot_X2 <-  ggplot2::ggplot(data = full_Data2, ggplot2::aes(x     = Time, 
                                                           y     = X2, 
                                                           color = as.factor(Individual), 
                                                           group = Individual)) +
                              ggplot2::geom_point() +
                              ggplot2::geom_line() +
                              ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                              ggplot2::ggtitle("Environment over time") +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment X2") +
                              ggplot2::theme(legend.position = "none")
  
  # Display environment X1X2
  plot_X1X2 <-  ggplot2::ggplot(data = full_Data2, ggplot2::aes(x     = Time, 
                                                             y     = X1X2, 
                                                             color = as.factor(Individual), 
                                                             group = Individual)) +
                                ggplot2::geom_point() +
                                ggplot2::geom_line() +
                                ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                ggplot2::ggtitle("Environment over time") +
                                ggplot2::xlab("Time") +
                                ggplot2::ylab("Environment X1X2") +
                                ggplot2::theme(legend.position = "none")
          
   myPlot <- list("X1"       = plot_X1, 
                  "X2"       = plot_X2,
                  "X1X2"     = plot_X1X2,
                  "totPhen"  = plot_TotPhen, 
                  "sampPhen" = plot_SampPhen, 
                  "sampTime" = plot_SampTime)
  
  return(myPlot)
}