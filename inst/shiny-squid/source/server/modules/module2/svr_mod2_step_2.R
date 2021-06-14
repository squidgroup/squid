#Server functions for module 2 step 2
c(
  ######### Display results (graph) #########
  
  get_bernoulli_data <- function(){
    
    probs <- seq(0, 1, length.out = 11)
    n     <- 1000
    out   <- lapply(probs, function(p) rbinom(n=n, size=1, prob=p)) 
    dat   <- data.frame("p"  = probs,
                        "mu" = sapply(out, mean),
                        "v"  = sapply(out, var))
    
    return(dat)
  },
  
  # Graph: mean-variance relationship in Bernoulli distribution
  output$Mod2Step2_plot_bernoulli_mean <- renderPlot({ 
    
    dat <- get_bernoulli_data()

    ggplot2::ggplot(data=dat, aes(x=factor(p), y=mu)) + 
      ggplot2::geom_col() + 
      xlab("Probability") +
      ylab("Mean")

  }),
  output$Mod2Step2_plot_bernoulli_var <- renderPlot({ 
    
    dat <- get_bernoulli_data()
    
    ggplot2::ggplot(data=dat, aes(x=factor(p), y=v)) +
      ggplot2::geom_col() +
      xlab("Probability") +
      ylab("Variance")
  
  }),
  
  
  get_poisson_data <- function(){
    
    lambdas <- c(0.5, 1, 3, 5, 7, 10, 20, 50, 75, 100)
    n       <- 1000
    out     <- lapply(lambdas, function(l) rpois(n=n, lambda=l)) 
    dat     <- data.frame("lambda"  = lambdas,
                          "mu"      = sapply(out, mean),
                          "v"       = sapply(out, var))
    
    return(dat)
  },
  
  # Graph: mean-variance relationship in Poisson distribution
  output$Mod2Step2_plot_poisson_mean <- renderPlot({ 
    
    dat <- get_poisson_data()
    
    ggplot2::ggplot(data=dat, aes(x=factor(lambda), y=mu)) + 
      ggplot2::geom_col() + 
      xlab("Lambda") +
      ylab("Mean")
    
  }),
  output$Mod2Step2_plot_poisson_var <- renderPlot({ 
    
    dat <- get_poisson_data()
    
    ggplot2::ggplot(data=dat, aes(x=factor(lambda), y=v)) +
      ggplot2::geom_col() +
      xlab("Lambda") +
      ylab("Variance")
    
  })
            
) # End
