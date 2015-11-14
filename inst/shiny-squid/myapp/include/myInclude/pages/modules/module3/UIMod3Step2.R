# UI: Module 3 Step 2

UIMod3Step2 <- function(){
  
  return(
    span( 
               
      h4(Mod3Step2_txt$title),           # Text: title
      
      p(HTML(Mod3Step2_txt$subgoal)),    # Text: subgoal
      p(HTML(Mod3Step2_txt$intro)),      # Text: introduction
      p(HTML(Mod3Step2_txt$exercise)),   # Text: exercise
      
      # inputs
      p("input >>>>> Vi"), 
      p("input >>>>> Ve"), 
      p("input >>>>> Vme"), 
      
      p(HTML(Mod3Step2_txt$para1)),      # Text: paragraph 1 
      
      # inputs
      p("input >>>>> Ve"), 
      p("input >>>>> shared/unshared"), 
      p("input >>>>> environment type"),
      
      p(HTML(Mod3Step2_txt$para2)),      # Text: paragraph 2 
      
      # inputs
      p("input >>>>> NR"), 
      p("input >>>>> Vit"), 
      
      p(HTML(Mod3Step2_txt$para3)),      # Text: paragraph 3
      
      p("RuNNNNNN"),  
    
      p(HTML(Mod3Step2_txt$results)),    # Text: results
      
      p(HTML(Mod3Step2_txt$para4)),      # Text: paragraph 4
      
      p('$$Y_j=I_0+ME_j$$'),
      
      p(HTML(Mod3Step2_txt$para5)),      # Text: paragraph 5
      
      p("table"),
      
      p(HTML(Mod3Step2_txt$conclusion)), # Text: conclusion
      
      p(HTML(Mod3Step2_txt$para6)),      # Text: paragraph 6
      
      p(HTML(Mod3Step2_txt$para7)),      # Text: paragraph 7
      
      p(HTML(Mod3Step2_txt$para8)),      # Text: paragraph 8
      
      div(class="line"),
      
      bsButton("Mod2Step2GotoStep1", label = "<< Previous Step (1)", style = Modules_VAR$StepLink$style), # Go to previous step       
      span(Modules_VAR$StepLink$sep, class="step-Link"),
      bsButton("Mod2Step2GotoStep3", label = "Next Step (3) >>", style = Modules_VAR$StepLink$style) # Go to next step
      
      
  
    )
  ) # End return
  
}