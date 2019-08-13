fixedPage(
  wellPanel( 

      h4(portal_txt$parag0_title),
      p(HTML(portal_txt$patag1_image), 
        HTML(portal_txt$parag0_contents_1)),
      p(HTML(portal_txt$parag0_contents_2)),
      
      h4("Guidance for Users"),
      fluidRow(                     
        column(width = 4,
          h5("For Beginners"),     
          p(HTML(portal_txt$beginners)) 
        ),
        column(width = 4,
          h5("For Teachers"), 
          p(HTML(portal_txt$teachers))
        ),
        column(width = 4,
          h5("For Experts"), 
          p(HTML(portal_txt$experts))
        )
      ),
      
      # HTML('<img src="pictures/modules.png" alt="SQuID">'),
      
      h4(portal_txt$parag3_title),
      p(HTML(portal_txt$parag3_contents1)),
      p(HTML(portal_txt$parag3_contents2)),
      
      h4(portal_txt$references_title),
      print_ref(bib[c("Allegue2016a", "Dingemanse2013a")])
    )      
)
