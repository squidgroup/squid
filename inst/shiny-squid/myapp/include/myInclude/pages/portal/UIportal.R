portal <- function(){
  
  return(
    fixedPage(
      wellPanel( 
      
          h4(portal_txt$parag1_title),
          p(HTML(portal_txt$parag1_contents)),
 
#           fluidRow(
#             column(8, p(HTML(portal_txt$parag1_contents))),
#             column(4, HTML('<img id="logo" src="pictures/logo_2.png" align="right" alt="SQuID">'))
#           ),
          
          h4(portal_txt$parag2_title),
          p(HTML(portal_txt$parag2_contents)),
          
          h4(portal_txt$parag3_title),
          p(HTML(portal_txt$parag3_contents1)),
          p(HTML(portal_txt$parag3_contents2)),
          
          
          h4(portal_txt$parag4_title),
          fluidRow(
            column(6, HTML('<img src="pictures/group_pic.jpg" alt="SQuID">')),
            column(6, p(HTML(portal_txt$parag4_contents)))
          )
        )      
    )
  )
  
}