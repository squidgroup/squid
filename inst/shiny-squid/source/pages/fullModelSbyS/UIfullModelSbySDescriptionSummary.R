
myTable <- getTable(data.frame(
  
  "Component"          = c("Component",
                           "$\\text{Fixed effects}$",
                           paste0("$",EQ3$mean0 ,"$"),
                           paste0("$",EQ3$mean1 ,EQ2$env1 ,"$"),
                           paste0("$",EQ3$mean2 ,EQ2$env2 ,"$"),
                           paste0("$",EQ3$mean12,EQ2$env12,"$"),
                           "$\\text{Random effects}$",
                           paste0("$",EQ3$dev0,"$"),
                           paste0("$",EQ3$dev1 ,EQ2$env1 ,"$"),
                           paste0("$",EQ3$dev2 ,EQ2$env2 ,"$"),
                           paste0("$",EQ3$dev12,EQ2$env12,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev1 ,EQ2$env1 ,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev2 ,EQ2$env2 ,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev12,EQ2$env12,"$"),
                           paste0("$",EQ3$dev1 ,EQ2$env1,"$ and $",EQ3$dev2 ,EQ2$env2 ,"$"),
                           paste0("$",EQ3$dev1 ,EQ2$env1,"$ and $",EQ3$dev12,EQ2$env12,"$"),
                           paste0("$",EQ3$dev2 ,EQ2$env2,"$ and $",EQ3$dev12,EQ2$env12,"$"),
                           paste0("$",NOT$groupV,"$"),
                           paste0("$",NOT$error,"$"),
                           paste0("$",NOT$trait.1,"$ or $",NOT$trait.2,"$")
  ),
  
  "Explanation"        = c("Explanation",
                           "",
                           "Population mean",
                           paste0("Population-average response to an environmental effect $",EQ2$env1,"$ (with variance $V_{",EQ2$env1,"}$)"),
                           paste0("Population-average response to an environmental effect $",EQ2$env2,"$ (with variance $V_{",EQ2$env2,"}$)"),
                           paste0("Population-average response interaction to two environmental effects ($",EQ2$env1,"$,$",EQ2$env2,"$)"),
                           "",
                           "Individual-specific deviations (random intercepts)",
                           paste0("Individual-specific response to an environmental effect $",EQ2$env1,"$ (random slopes)"),
                           paste0("Individual-specific response to an environmental effect $",EQ2$env2,"$ (random slopes)"),
                           paste0("Individual-specific response to two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$ (random slopes)"),
                           paste0("Covariance between random intercepts and random-slopes in response to an environmental effect $",EQ2$env1,"$."),
                           paste0("Covariance between random intercepts and random-slopes in response to an environmental effect $",EQ2$env2,"$."),
                           paste0("Covariance between random intercepts and individual-specific response interaction to two environmental effects $(",EQ2$env1,",",EQ2$env2,")$ (random slopes)."),
                           paste0("Covariance between random-slopes in response to an environmental effect $",EQ2$env1,"$ and random-slopes in response to an environmental effect $",EQ2$env2,"$."),
                           paste0("Covariance between random-slopes in response to an environmental effect $",EQ2$env1,"$ and individual-specific response interaction to two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$."),
                           paste0("Covariance between random-slopes in response to an environmental effect $",EQ2$env2,"$ and individual-specific response interaction to two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$."),
                           paste0("Higher-level grouping variance (clusters, groups, families etc.)"),
                           paste0("Measurement error"),
                           paste0("Total phenotypic variance")
  ),
  
  "Variance.component" = c("Variance component $^b$",
                           "",
                           "-",
                           paste0("$V_{",EQ3$mean1 ,"}=",EQ3$mean1 ,"^2V_{",EQ2$env1,"}$"),
                           paste0("$V_{",EQ3$mean2 ,"}=",EQ3$mean2 ,"^2V_{",EQ2$env2,"}$"),
                           paste0("$V_{",EQ3$mean12,"}=",EQ3$mean12,"^2V_{",EQ2$env12,"}$"),
                           "",
                           paste0("$V_",EQ3$dev0,"=Var(",EQ3$dev0,")$"),
                           paste0("$V_{",EQ3$dev1 ,"} = Var(",EQ3$dev1 ,")V_{",EQ2$env1 ,"}+E(",EQ2$env1 ,")^2.Var(",EQ3$dev1 ,")$"),
                           paste0("$V_{",EQ3$dev2 ,"} = Var(",EQ3$dev2 ,")V_{",EQ2$env2 ,"}+E(",EQ2$env2 ,")^2.Var(",EQ3$dev2 ,")$"),
                           paste0("$V_{",EQ3$dev12,"} = Var(",EQ3$dev12,")V_{",EQ2$env12,"}+E(",EQ2$env12,")^2.Var(",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev1 ,"}=E(",EQ2$env1 ,").Cov(",EQ3$dev0,",",EQ3$dev1 ,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev2 ,"}=E(",EQ2$env2 ,").Cov(",EQ3$dev0,",",EQ3$dev2 ,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev12,"}=E(",EQ2$env12,").Cov(",EQ3$dev0,",",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev1,",",EQ3$dev2,"}=E(",EQ2$env1,").E(",EQ2$env2,").Cov(",EQ3$dev1,",",EQ3$dev2,")$"),
                           paste0("$COV_{",EQ3$dev1,",",EQ3$dev12,"}=E(",EQ2$env1,").E(",EQ2$env12,").Cov(",EQ3$dev1,",",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev2,",",EQ3$dev12,"}=E(",EQ2$env2,").E(",EQ2$env12,").Cov(",EQ3$dev2,",",EQ3$dev12,")$"),
                           paste0("$V_",NOT$groupV,"=Var(",NOT$groupV,")$"),
                           paste0("$V_",NOT$error,"=Var(",NOT$error,")$"),
                           paste0("$V_",NOT$total,"$")
  ),
  
  "Remarks"            = c("Remarks",
                           "",
                           "-",
                           paste0("In SQuID $V_{",EQ2$env1,"}=1$."),
                           paste0("In SQuID $V_{",EQ2$env2,"}=1$."),
                           paste0("Since in SQuID $V_{",EQ2$env1,"}=V_{",EQ2$env2,"}=1$ and $",EQ2$env1,"$ and $",EQ2$env2,"$ are independent of each other, the expected variance of the product is $V_{",EQ2$env12,"}=1$. $^c$"),
                           "",
                           paste0("In the presence of random slope variation, $V_",EQ3$dev0,"$ expresses the variance at the point where all covariates are zero. Since all covariates are centred to zero in SQuID, this represents the variance at average values of the covariate(s)."),
                           paste0("In SQuID $V_{",EQ2$env1,"}=1$ and $E(",EQ2$env1,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev1,"}=Var(",EQ3$dev1,")$."),
                           paste0("In SQuID $V_{",EQ2$env2,"}=1$ and $E(",EQ2$env2,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev2,"}=Var(",EQ3$dev2,")$."),
                           paste0("In SQuID $V_{",EQ2$env1,"}=V_{",EQ2$env2,"}=1$, $E(",EQ2$env1,")=E(",EQ2$env2,")=0$, $",EQ2$env1,"$ and $",EQ2$env2,"$ are independent of each other, the expected variance of the product $V_{",EQ2$env12,"}=1$ and the expected mean of the product is $E(",EQ2$env12,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev12,"}=Var(",EQ3$dev12,")$. $^d$"),
                           paste0("In SQuID $E(",EQ2$env1,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           paste0("In SQuID $E(",EQ2$env2,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^e$"),
                           "-",
                           "-",
                           "-"
  )
  
 ), header=TRUE)

span(
  p(fullmodelTxt$summaryStepbyStep),
  div(class="line"),
  h4("Phenotypic equation"),
  source("./source/pages/fullModelSbyS/UIfullModelEquation_2.R",local=TRUE)[["value"]],
  div(class="line"),
  h4(HTML("Summation of variance components<sup>a</sup>")),
  source("./source/pages/fullModelSbyS/UIfullModelVarianceEquation.R",local=TRUE)[["value"]],
  div(class="line"),
  myTable,
  div(class="line"),
  p(HTML("<sup>a</sup> Covariance parameters exist but do not contribute to total phenotypic variance.")),
  p(HTML("<sup>b</sup> Variances as they contribute to the total phenotypic variance.")),
  p(HTML(paste0("<sup>c</sup> We anticipate that the covariance $Cov_{",EQ2$env1,",",EQ2$env2,"}$ between $",EQ2$env1,"$ and $",EQ2$env1,"$ can be set by the user while SQuID evolves, which will affect $V_{",EQ2$env1,EQ2$env2,"}$."))),
  p(HTML(paste0("<sup>d</sup> We anticipate that the covariance $Cov_{",EQ2$env1,",",EQ2$env2,"}$ between $",EQ2$env1,"$ and $",EQ2$env1,"$ can be set by the user while SQuID evolves, which will affect $V_{",EQ2$env1,EQ2$env2,"}$ and $E(",EQ2$env1,EQ2$env2,")$."))),
  p(HTML(paste0("<sup>e</sup> Note the distinction between $COV_{",EQ3$dev0,",",EQ3$dev1,"}$ as a potential contributor to the variance and $Cov(",EQ3$dev0,",",EQ3$dev1,")$ as a covariance between intercepts and slopes, and that it can be simulated and estimated. 
                Mean centring of the environmental gradients has the advantage that we can interpret the intercept variance as the variance at an average environmental value and the intercept-slope covariance as the location of the minimum of the between-individual variance. 
                With arbitrary scaling of the environmental gradients, the interpretation of the intercept variance will change and $COV_{",EQ3$dev0,",",EQ3$dev1,"}$ will have to appear in the summation of variance components.")))
)








