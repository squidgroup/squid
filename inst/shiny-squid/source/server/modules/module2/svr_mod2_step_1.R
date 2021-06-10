#Server functions for module 2 step 1
c(
  ######### Display results (graph) #########
  
  # Graph: proportion of each sex
  output$Mod2Step1_plot_alligator <- renderPlot({ 
    
    t    <- input$Mod2Step1_temperature
    prop <- ifelse(t < 30, 0, 
            ifelse(t > 33, 1, (t-30)/(33-30))) 
  
    dat <- data.frame("Sex"        = c("Female", "Male"),
                      "Proportion" = c(1-prop, prop))
  
    ggplot2::ggplot(data=dat, aes(x=Sex, y=Proportion)) + 
      ggplot2::geom_col(width=0.3) +
      ggplot2::ylim(0,1)
    
  }),
  
  
  # Graph: proportion of each sex
  output$Mod2Step1_plot_coin_flip <- renderPlot({ 
    
    if(input$Mod2Step1_Refresh_1 == 0){}
    
    size <- input$Mod2Step1_n_offspring
    prop <- rbinom(n=1, size=size, prob=0.5) / size
    dat  <- data.frame("Sex"        = c("Female", "Male"),
                       "Proportion" = c(1-prop, prop))
    
    ggplot2::ggplot(data=dat, aes(x=Sex, y=Proportion)) + 
      ggplot2::geom_col(width=0.3) +
      ggplot2::ylim(0,1) + 
      ggplot2::geom_hline(yintercept=0.5, color="red", linetype="dashed")
    
  }),
  
  # Graph: proportion of each sex
  output$Mod2Step1_plot_female_prob <- renderPlot({ 
    
    if(input$Mod2Step1_Refresh_2 == 0){}
    
    f_prob <- input$Mod2Step1_female_probability
    
    prop <- rbinom(n=1, size=100, prob=f_prob) / 100
    dat <- data.frame("Sex"        = c("Female", "Male"),
                      "Proportion" = c(prop, 1-prop))
    
    ggplot2::ggplot(data=dat, aes(x=Sex, y=Proportion)) + 
      ggplot2::geom_col(width=0.3) +
      ggplot2::ylim(0,1) + 
      ggplot2::geom_hline(yintercept=f_prob, color="red", linetype="dashed")
    
  }),
  
  # Graph: histogram of female proportions
  output$Mod2Step1_plot_female_hist <- renderPlot({ 
    
    size <- input$Mod2Step1_n_offspring_2
    prob <- input$Mod2Step1_female_probability_2
    prop <- rbinom(n=1000, size=size, prob=prob) / size
    dat  <- data.frame("Proportion" = prop)
    
    ggplot2::ggplot(data=dat, aes(x=Proportion)) + 
      ggplot2::geom_histogram(binwidth=0.1) +
      ggplot2::xlim(-0.1,1.1) + 
      ggplot2::ylim(0, 1000) +
      ggplot2::xlab("Female proportion") +
      ggplot2::geom_vline(xintercept=prob, color="red", linetype="dashed")
    
  }),
  
  # Graph: histogram of counts
  output$Mod2Step1_plot_count_hist <- renderPlot({ 
    
    rate   <- input$Mod2Step1_poisson_rate
    counts <- rpois(n=1000, lambda=rate)
    dat    <- data.frame("Counts" = counts)
    
    ggplot2::ggplot(data=dat, aes(x=Counts)) + 
      ggplot2::geom_histogram(binwidth=0.5) +
      ggplot2::geom_vline(xintercept=rate, color="red", linetype="dashed")
    
  })
            
) # End
