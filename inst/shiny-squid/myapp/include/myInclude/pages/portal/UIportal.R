portal <- function(){
  
  return(
    fixedPage(
      wellPanel( 
        
          h4(portal_txt$parag1_title),
          p(HTML(portal_txt$parag1_contents)),
          
          h4(portal_txt$parag3_title),
          p(HTML(portal_txt$parag3_contents1)),
          p(HTML(portal_txt$parag3_contents2))
        )      
    )
  )
  
}