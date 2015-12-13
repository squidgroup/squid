### Display results

display_results <- function(N, Time, data_C, data_S){ 
    
  data_C <- data_C %>%
              dplyr::filter(Trait == 1, Replicate == 1, Individual <= 20)
  
  data_S <- data_S %>%
              dplyr::filter(Trait == 1, Replicate == 1, Individual <= 20)
  
  # Display phenotype
  plot_TotPhen <- qplot(Time, 
                  Phenotype, 
                  data=data_C, 
                  color=as.factor(Individual),
                  xlim=c(Time$Tmin, Time$Tmax)) + 
    geom_point() + 
    geom_line()  +
    ggtitle("Total individual phenotypes") +
    xlab("Time") +
    ylab("Phenotype") +
    theme(legend.position="none") +
    geom_vline(xintercept = c(Time$Ts, Time$Te))
  
  #Display sampling phenotype
  plot_SampPhen <- qplot(Time, 
                  Phenotype, 
                  data=data_S, 
                  color=as.factor(Individual), 
                  xlim=c(Time$Tmin, Time$Tmax)) + 
    geom_point() + 
    geom_line()  +
    ggtitle("Sampled individual phenotypes") +
    xlab("Time") +
    ylab("Phenotype") +
    theme(legend.position="none") +
    geom_vline(xintercept = c(Time$Ts, Time$Te))
  
  # Display sampling variation
  plot_SampTime <- qplot(Time, 
                  Individual_Trait, 
                  data=data_S, 
                  color=as.factor(Individual),
                  xlim=c(Time$Tmin, Time$Tmax)) + 
    ggtitle("Sampling time per individual") +
    xlab("Time") +
    ylab("Individuals") +
    theme(legend.position="none") +
    geom_vline(xintercept = c(Time$Ts, Time$Te))
  
  # Display environment X1
  plot_X1 <-  ggplot(data=data_C, aes(x=Time, y=X1, color=Individual, group=Individual)) +
    # geom_line() +
    geom_point() +
    xlim(Time$Tmin, Time$Tmax) + 
    ggtitle("Environment over time") +
    xlab("Time") +
    ylab("Environment X1") +
    theme(legend.position="none")
  
  # Display environment X2
  plot_X2 <-  ggplot(data=data_C, aes(x=Time, y=X2, color=Individual, group=Individual)) +
    # geom_line() +
    geom_point() +
    xlim(Time$Tmin, Time$Tmax) + 
    ggtitle("Environment over time") +
    xlab("Time") +
    ylab("Environment X2") +
    theme(legend.position="none")
  
  # Display environment X1X2
  plot_X1X2 <-  ggplot(data=data_C, aes(x=Time, y=X1X2, color=Individual, group=Individual)) +
    # geom_line() +
    geom_point() +
    xlim(Time$Tmin, Time$Tmax) + 
    ggtitle("Environment over time") +
    xlab("Time") +
    ylab("Environment X1X2") +
    theme(legend.position="none")
  
   myPlot <- list("plotX1"=plot_X1, 
                  "plotX2"=plot_X2,
                  "plotX1X2"=plot_X1X2,
                  "plotTotPhen"=plot_TotPhen, 
                  "plotSampPhen"=plot_SampPhen, 
                  "plotSampTime"=plot_SampTime)

  return(myPlot)
}