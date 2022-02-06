div(
  wellPanel(
  
      h4(portal_txt$parag0_title),
      div(p(HTML(portal_txt$patag1_image),
        HTML(portal_txt$parag0_contents_1)),
      p(HTML(portal_txt$parag0_contents_2)))
  ),
  wellPanel(
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
      )
  ),
  wellPanel(
      # HTML('<img src="pictures/modules.png" alt="SQuID">'),
  
      h4(portal_txt$parag3_title),
      p(HTML(portal_txt$parag3_contents1)),
      p(HTML(portal_txt$parag3_contents2))
  ),
  wellPanel(
      p(strong("References:")),
      p(HTML("Allegue, H., Araya-Ajoy, Y. G., Dingemanse, N. J., Dochtermann, N. A., Garamszegi, L. Z., 
              Nakagawa, S., Réale, D., Schielzeth, H.& Westneat, D. F. (2016) Statistical Quantification of 
              Individual Differences: an educational and statistical tool for understanding multi-level phenotypic 
              data in linear mixed models. <i>Methods in Ecology and Evolution</i>, 8, 257-267.
              <a href='https://doi.org/10.1111/2041-210X.12659' target='_blank'>doi: 10.1111/2041-210X.12659</a>")),
      p(HTML("Dingemanse, N. J.& Dochtermann, N. A. (2013) Quantifying individual variation in behaviour: 
              mixed-effect modelling approaches. <i>Journal of Animal Ecology</i>, 82, 39-54.
              <a href='https://doi.org/10.1111/1365-2656.12013' target='_blank'>doi: 10.1111/1365-2656.12013</a>"))
  )
)