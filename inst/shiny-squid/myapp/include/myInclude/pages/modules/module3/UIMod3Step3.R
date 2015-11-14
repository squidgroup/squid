# UI: Module 3 Step 3

UIMod3Step3 <- function(){
  
  return(
    span( 
               
      h4(Mod3Step3_txt$title),           # Text: title
      
      p(HTML(Mod3Step3_txt$subgoal)),    # Text: subgoal
      p(HTML(Mod3Step3_txt$intro)),      # Text: introduction
      p(HTML(Mod3Step3_txt$para1)),      # Text: paragraph 1 
      p(HTML(Mod3Step3_txt$exercise)),   # Text: exercise
      
      # inputs
      p("input >>>>> Vi"), 
      p("input >>>>> Ve"), 
      p("input >>>>> Vme"), 
      
      p(HTML(Mod3Step3_txt$para2)),      # Text: paragraph 2 
      
      # inputs
      p("input >>>>> Ve"), 
      p("input >>>>> shared/unshared"), 
      p("input >>>>> environment type"),
      
      p(HTML(Mod3Step3_txt$para3)),      # Text: paragraph 3
      
      # inputs
      p("input >>>>> Beta 1"), 
      p("input >>>>> Beta 2"), 
      p("input >>>>> NR"), 
      p("input >>>>> Vit"), 
      
      p("RuNNNNNN"),  
      
      p(HTML(Mod3Step3_txt$results)),         # Text: results
      
      p('$$Y_j=I_0+\\beta_{ES}E_S+ME_j$$'),
      
      p(HTML(Mod3Step3_txt$para4)),      # Text: paragraph 4
      
      p('$$Y_ij=I_0j+\\beta_1\\overline{E}_j+\\beta_2(\\overline{E}_j-E_{ij})+ME_j$$'),
      
      p(HTML(Mod3Step3_txt$para5)),      # Text: paragraph 5
      
      p(HTML(Mod3Step3_txt$para6)),      # Text: paragraph 6
      
      p("table"),
      
      p("output >>>>> graph"),
      
      p(HTML(Mod3Step3_txt$para7)),      # Text: paragraph 7
      
      p(HTML(Mod3Step3_txt$conclusion)), # Text: conclusion
      
      p(HTML(Mod3Step3_txt$para8)),      # Text: paragraph 8
      
      p(HTML(Mod3Step3_txt$finalcaveat)),# Text: a final caveat
      
      p(HTML(Mod3Step3_txt$para9)),      # Text: paragraph 9
      
      div(class="line"),
      
      bsButton("Mod3Step4GotoStep3", label = "<< Previous Step (2)", style="link") # Go to previous step     
    
    )
  ) # End return
  
}