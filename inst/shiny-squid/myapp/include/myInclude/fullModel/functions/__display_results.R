### Display results

display_results <- function(N, Time, data_C, data_S){ 
    
  # Display phenotype
  plot_TotPhen <- qplot(Time, 
                  Phenotype, 
                  data=data_C, 
                  color=Individual, 
                  group=Individual_Trait, 
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
                  color=Individual, 
                  group=Individual_Trait, 
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
                  color=Individual,
                  xlim=c(Time$Tmin, Time$Tmax)) + 
    ggtitle("Sampling time per individual") +
    xlab("Time") +
    ylab("Individuals") +
    theme(legend.position="none") +
    geom_vline(xintercept = c(Time$Ts, Time$Te))
  
  # Display environment X1
#   plot_X1 <- qplot(Time,
#                   X1,
#                   data=data_C, 
#                   color=Individual, 
#                   group=Individual,
#                   xlim=c(Time$Tmin, Time$Tmax)) + 
#     ggtitle("Environment over time") +
#     xlab("Time") +
#     ylab("Environment X1") +
#     theme(legend.position="none")
  
  plot_X1 <-  ggplot(data=data_C, aes(x=Time, y=X1, color=Individual, group=Individual)) +
    # geom_line() +
    geom_point() +
    xlim(Time$Tmin, Time$Tmax) + 
    ggtitle("Environment over time") +
    xlab("Time") +
    ylab("Environment X1") +
    ylab("Environment") +
    theme(legend.position="none")
  
  
  
  # Display environment X2
  plot_X2 <- qplot(Time, 
                  X2, 
                  data=data_C, 
                  color=Individual, 
                  group=Individual, 
                  xlim=c(Time$Tmin, Time$Tmax)) +   
    xlab("Time") +
    ylab("Environment X2") +
    theme(legend.position="none")
  
  # Display environment X1X2
  plot_X1X2 <- qplot(Time, 
                   X1X2, 
                   data=data_C, 
                   color=Individual, 
                   group=Individual, 
                   xlim=c(Time$Tmin, Time$Tmax)) +   
    xlab("Time") +
    ylab("Environment X1X2") +
    theme(legend.position="none")
  
#   # Display environment EG
#   plot_EG <- qplot(time, 
#                    EG, 
#                    data=data_C, 
#                    color=individual, 
#                    group=id_ind_trait, 
#                    xlim=c(Time$Tmin, Time$Tmax)) +   
#     xlab("Time") +
#     ylab("Environment EG") +
#     theme(legend.position="none")
#   
#   # Display environment ES
#   plot_ES <- qplot(time, 
#                    ES, 
#                    data=data_C, 
#                    color=individual, 
#                    group=id_ind_trait, 
#                    xlim=c(Time$Tmin, Time$Tmax)) +   
#     xlab("Time") +
#     ylab("Environment ES") +
#     theme(legend.position="none")
  
   myPlot <- list("plotX1"=plot_X1, 
                  "plotX2"=plot_X2,
                  "plotX1X2"=plot_X1X2,
#                   "plotEG"=plot_EG,
#                   "plotES"=plot_ES,
                  "plotTotPhen"=plot_TotPhen, 
                  "plotSampPhen"=plot_SampPhen, 
                  "plotSampTime"=plot_SampTime)
  
  #####################################################
  
#   if(N$NT==2){
#     
#     require(devtools)
#     require(ggplot2)
#     
#     plot_1 <- qplot(x, 
#                     Phenotype, 
#                     data=data_C,
#                     colour=as.factor(id_ind),
#                     main="", 
#                     xlab="x", ylab="phenotype",
#                     group=id_ind_trait) + 
#       theme(legend.position="none")
#     
#     plot_2 <- qplot(subset(data_C, Trait == 1)$Phenotype, 
#                     subset(data_C, Trait == 2)$Phenotype, 
#                     colour=subset(data_C, Trait == 1)$id_ind, 
#                     main="", 
#                     xlab="Trait 1", ylab="Trait 2") + 
#       theme(legend.position="none")
#     
#     multiplot(plot_1,plot_2,cols=1)
#     
#   }

  return(myPlot)
}