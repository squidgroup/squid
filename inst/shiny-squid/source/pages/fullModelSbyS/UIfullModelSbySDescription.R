
myTable <-  data.frame(
              "Components"   = c(paste("$",NOT$time,"$",sep=""), 
                                 paste("$",NOT$ind,"$",sep=""),  
                                 paste("$",NOT$group,"$",sep=""), 
                                 paste("$",EQ$phen.1,"$", sep=""),
                                 paste("$",EQ$phen.2,"$", sep=""),
                                 paste("$",NOT$mean,"_0$",sep=""), 
                                 paste("$",NOT$devI,"_",NOT$ind,"$",sep=""), 
                                 paste("$",NOT$mean," ",NOT$env,"$",sep=""),
                                 paste("$",NOT$devS,"_",NOT$ind,NOT$env,"$",sep=""),
                                 paste("$",NOT$groupV,"_",NOT$group,"$",sep=""),
                                 paste("$",NOT$error,"_{",NOT$time,NOT$ind,NOT$group,"}$",sep="")
                                ),
              "Explanations" = c("Instance of time",
                                "Individual identity",
                                "Group identity",
                                paste("Phenotypic value of trait $",NOT$trait.1,"$ at instance $",NOT$time,"$ for individual $",NOT$ind,"$ within group $",NOT$group,"$.",sep=""),
                                paste("Phenotypic value of trait $",NOT$trait.2,"$ at instance $",NOT$time,"$ for individual $",NOT$ind,"$ within group $",NOT$group,"$.",sep=""),
                                "Population mean",
                                paste("Individual-specific deviations (random-intercepts) for individual $",NOT$ind,"$.",sep=""),
                                paste("Population mean response to environmental influences $",NOT$env,"$.",sep=""),
                                paste("Individual-specific response to environmental influences $",NOT$env,"$ (random-slope) for individual $",NOT$ind,"$.",sep=""),
                                paste("Higher-level grouping for each group $",NOT$group,"$.",sep=""),
                                paste("Measurement error for the individual $",NOT$ind,"$ and at time $",NOT$time,"$.",sep="")
              )
)

myDefinitionTable    <- getTable(myTable)

myVarCovMatrix       <- paste(
"$$ \\begin{pmatrix}",
        EQ$dev0.1 , "\\\\",
        EQ$dev1.1 , "\\\\",
        EQ$dev2.1 , "\\\\",
        EQ$dev12.1, "\\\\",
        EQ$dev0.2 , "\\\\",
        EQ$dev1.2 , "\\\\",
        EQ$dev2.2 , "\\\\",
        EQ$dev12.2, "\\\\
    \\end{pmatrix}
        \\sim MNV(0,\\Omega_{",NOT$devI, NOT$devS,"}):
        \\Omega_{",NOT$devI, NOT$devS,"}=
    \\begin{pmatrix}
        V_{",   EQ2$dev0.1,"}                  &                                      &                                    &                                     &                                    &                                    &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev1.1 ,"} & V_{",   EQ2$dev1.1, "}                &                                    &                                     &                                    &                                    &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev2.1 ,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev2.1 ,"} & V_{",  EQ2$dev2.1,"}                &                                     &                                    &                                    &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev12.1,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev12.1,"} & Cov_{",EQ2$dev2.1,",",EQ2$dev12.1,"} & V_{",  EQ2$dev12.1,"}                &                                    &                                    &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev0.2 ,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev0.2 ,"} & Cov_{",EQ2$dev2.1,",",EQ2$dev0.2 ,"} & Cov_{",EQ2$dev12.1,",",EQ2$dev0.2 ,"} & V_{",  EQ2$dev0.2,"}                &                                    &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev1.2 ,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev1.2 ,"} & Cov_{",EQ2$dev2.1,",",EQ2$dev1.2 ,"} & Cov_{",EQ2$dev12.1,",",EQ2$dev1.2 ,"} & Cov_{",EQ2$dev0.2,",",EQ2$dev1.2 ,"} & V_{",EQ2$dev1.2,"}                  &                                    &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev2.2 ,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev2.2 ,"} & Cov_{",EQ2$dev2.1,",",EQ2$dev2.2 ,"} & Cov_{",EQ2$dev12.1,",",EQ2$dev2.2 ,"} & Cov_{",EQ2$dev0.2,",",EQ2$dev2.2 ,"} & Cov_{",EQ2$dev1.2,",",EQ2$dev2.2 ,"} & V_{",EQ2$dev2.2,"}                  &                    \\\\
        Cov_{", EQ2$dev0.1, ",", EQ2$dev12.2,"} & Cov_{", EQ2$dev1.1, ",",EQ2$dev12.2,"} & Cov_{",EQ2$dev2.1,",",EQ2$dev12.2,"} & Cov_{",EQ2$dev12.1,",",EQ2$dev12.2,"} & Cov_{",EQ2$dev0.2,",",EQ2$dev12.2,"} & Cov_{",EQ2$dev1.2,",",EQ2$dev12.2,"} & Cov_{",EQ2$dev2.2,",",EQ2$dev12.2,"} & V_{",EQ2$dev12.2,"} \\\\
    \\end{pmatrix} 
$$",sep="")

myK      <- paste("$$",NOT$groupV,"_{",NOT$group,"} \\sim N(0,\\sqrt{V_",NOT$groupV,"})$$", sep="")
myM      <- paste("$$",NOT$error,"_{",NOT$time,NOT$ind,NOT$group,"} \\sim N(0,\\sqrt{V_",NOT$error,"})$$", sep="")

c(

  # Description page 
  p(HTML(fullmodelTxt$ModelDesciption_intro_1)),

  tabsetPanel(id = "FModSbyS_Description_TabsetPanel", type = "tabs", 
    
    tabPanel("Simulation model",           
              
      p(HTML(fullmodelTxt$SimModel_intro)),   
             
      h4("Model equation"),
      
      source("./source/pages/fullModelSbyS/UIfullModelEquation.R",local=TRUE)[["value"]],
     
      myDefinitionTable,
      
      p(HTML(fullmodelTxt$modelEquation_1)),
      p(HTML(fullmodelTxt$modelEquation_2)),
      p(HTML(fullmodelTxt$modelEquation_3)),
      p(HTML(fullmodelTxt$modelEquation_4)),
      p(HTML(fullmodelTxt$modelEquation_5)),
      p(HTML(fullmodelTxt$modelEquation_6)),
      p(HTML(fullmodelTxt$modelEquation_7)),
      
      h4("Individual specific responses"),
      p(HTML(fullmodelTxt$indSpecResponses)),
      myVarCovMatrix,
      
      h4("Environments"),
      p(HTML(fullmodelTxt$environment_1)),
      p(HTML(fullmodelTxt$environment_2)),
      p(HTML(fullmodelTxt$environment_3)),
      p(HTML(fullmodelTxt$environment_4)),
      p(HTML(fullmodelTxt$environment_5)),
      p(HTML(fullmodelTxt$environment_6)),
      p(HTML('<figure>
              <img src="pictures/Environment_examples.jpg" alt="The Pulpit Rock">
              <figcaption><b>Figure 1:</b> example of 3 environmental effects. 
                                      (Left) Linear trend with some stochasticity 
                                      (Middle) Cyclic pattern with linear trend and some stochasticity. 
                                      (Right) Autocorrelation of 0.7 added to stochastic values.</figcaption>
              </figure>')
        ),
      p(HTML(fullmodelTxt$environment_7)),
      
      h4("High-level grouping and measurement error"),
      p(HTML(fullmodelTxt$groupingNerror)),
      myK,
      myM
    
    ), # End Simulation model
    
    tabPanel("Sampling design", 
             
      p(HTML(fullmodelTxt$samplingDesign_1)),
      p(HTML(fullmodelTxt$samplingDesign_2)),
      
      p(HTML(
        paste('<figure>
          <img src="pictures/Vit_examples.jpg" alt="The Pulpit Rock">
          <figcaption><b>Figure 1:</b> example of 2 sampling designs where the among-individual variance in timing of sampling ($',general_VAR$btwIndVarTimSamp,'$) is 0.1 (left) and 0.9 (right).</figcaption>
          </figure>',sep=""))
        ),
      
      p(HTML(fullmodelTxt$samplingDesign_3)),
      p(HTML(fullmodelTxt$samplingDesign_4))
             
    ) # End Sampling design
  )
)