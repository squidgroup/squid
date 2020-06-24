ggplotCustomTheme <- function() {

  (ggplot2::theme_bw() +
   ggplot2::theme(legend.position = "none",
                  plot.title      = ggplot2::element_text(hjust = 0.5))) 
  
}

defaultPlot <- function(){print(NULL)}