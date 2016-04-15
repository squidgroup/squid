### Display results
displayResults <- function(N, Time, full_Data, sampled_Data){ 
    
  # subset data
  full_Data2    <- copy(data.table::as.data.table(full_Data))[   Trait == 1 & Replicate == 1 & Individual <= 20]
  sampled_Data2 <- copy(data.table::as.data.table(sampled_Data))[Trait == 1 & Replicate == 1 & Individual <= 20]
  
  # Display phenotype
  plot_TotPhen <-  ggplot2::ggplot(data=full_Data2, ggplot2::aes(x     = Time, 
                                                                y     = Phenotype, 
                                                                color = as.factor(Individual), 
                                                                group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Total individual phenotypes") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Phenotype") +
                                    ggplot2::theme(legend.position="none")
  

  #Display sampling phenotype
  plot_SampPhen <-  ggplot2::ggplot(data=sampled_Data2, ggplot2::aes(x     = Time, 
                                                                    y     = Phenotype, 
                                                                    color = as.factor(Individual), 
                                                                    group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::geom_line() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Sampled individual phenotypes") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Phenotype") +
                                    ggplot2::theme(legend.position="none")
  
  # Display sampling time
  plot_SampTime <-  ggplot2::ggplot(data=sampled_Data2, ggplot2::aes(x     = Time, 
                                                                    y     = Individual_Trait, 
                                                                    color = as.factor(Individual), 
                                                                    group = Individual)) +
                                    ggplot2::geom_point() +
                                    ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                    ggplot2::ggtitle("Sampling time per individual") +
                                    ggplot2::xlab("Time") +
                                    ggplot2::ylab("Individuals") +
                                    ggplot2::theme(legend.position="none")
  
  # Display environment X1
  plot_X1 <-  ggplot2::ggplot(data=full_Data2, ggplot2::aes(x     = Time, 
                                                           y     = X1, 
                                                           color = as.factor(Individual), 
                                                           group = Individual)) +
                              ggplot2::geom_point() +
                              ggplot2::geom_line() +
                              ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                              ggplot2::ggtitle("Environment over time") +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment X1") +
                              ggplot2::theme(legend.position="none")
  
  # Display environment X2
  plot_X2 <-  ggplot2::ggplot(data=full_Data2, ggplot2::aes(x     = Time, 
                                                           y     = X2, 
                                                           color = as.factor(Individual), 
                                                           group = Individual)) +
                              ggplot2::geom_point() +
                              ggplot2::geom_line() +
                              ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                              ggplot2::ggtitle("Environment over time") +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment X2") +
                              ggplot2::theme(legend.position="none")
  
  # Display environment X1X2
  plot_X1X2 <-  ggplot2::ggplot(data=full_Data2, ggplot2::aes(x     = Time, 
                                                             y     = X1X2, 
                                                             color = as.factor(Individual), 
                                                             group = Individual)) +
                                ggplot2::geom_point() +
                                ggplot2::geom_line() +
                                ggplot2::xlim(Time$Tmin, Time$Tmax) + 
                                ggplot2::ggtitle("Environment over time") +
                                ggplot2::xlab("Time") +
                                ggplot2::ylab("Environment X1X2") +
                                ggplot2::theme(legend.position="none")
          
   myPlot <- list("X1"       = plot_X1, 
                  "X2"       = plot_X2,
                  "X1X2"     = plot_X1X2,
                  "totPhen"  = plot_TotPhen, 
                  "sampPhen" = plot_SampPhen, 
                  "sampTime" = plot_SampTime)
  
  return(myPlot)
}