
myTable <- getTable(data.frame(
  
  "Component"          = c("Component",
                           "$\\text{Fixed effects}$",
                           paste0("$",EQ3$mean0 ,"$"),
                           paste0("$",EQ3$mean1 ,"$"),
                           paste0("$",EQ3$mean2 ,"$"),
                           paste0("$",EQ3$mean12,"$"),
                           "$\\text{Random effects}$",
                           paste0("$",EQ3$dev0,"$"),
                           paste0("$",EQ3$dev1 ,"$"),
                           paste0("$",EQ3$dev2 ,"$"),
                           paste0("$",EQ3$dev12,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev1 ,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev2 ,"$"),
                           paste0("$",EQ3$dev0,"$ and $",EQ3$dev12,"$"),
                           paste0("$",EQ3$dev1 ,"$ and $",EQ3$dev2 ,"$"),
                           paste0("$",EQ3$dev1 ,"$ and $",EQ3$dev12,"$"),
                           paste0("$",EQ3$dev2 ,"$ and $",EQ3$dev12,"$"),
                           paste0("$",NOT$groupV,"$"),
                           paste0("$",NOT$error,"$"),
                           paste0("$",NOT$trait.1,"$ or $",NOT$trait.2,"$")
  ),
  
  "Explanation"        = c("Explanation",
                           "",
                           "Population mean",
                           paste0("Population-average response to an environmental effect $",EQ2$env1,"$ (with variance $Var(",EQ2$env1,")$)"),
                           paste0("Population-average response to an environmental effect $",EQ2$env2,"$ (with variance $Var(",EQ2$env2,")$)"),
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
                           paste0("Residual (unaccounted effect on the phenotype) $^e$"),
                           paste0("Total phenotypic variance")
  ),
  
  "Variance.component" = c("Variance component $^b$",
                           "",
                           "-",
                           paste0("$V_{",EQ3$mean1 , EQ2$env1 ,"}=",EQ3$mean1 ,"^2Var(",EQ2$env1,")$"),
                           paste0("$V_{",EQ3$mean2 , EQ2$env2 ,"}=",EQ3$mean2 ,"^2Var(",EQ2$env2,")$"),
                           paste0("$V_{",EQ3$mean12, EQ2$env12,"}=",EQ3$mean12,"^2Var(",EQ2$env12,")$"),
                           "",
                           paste0("$V_",EQ3$dev0,"=Var(",EQ3$dev0,")$"),
                           paste0("$V_{",EQ3$dev1 , EQ2$env1, "} = Var(",EQ3$dev1 ,")Var(",EQ2$env1 ,")+E(",EQ2$env1 ,")^2.Var(",EQ3$dev1 ,")$"),
                           paste0("$V_{",EQ3$dev2 , EQ2$env2, "} = Var(",EQ3$dev2 ,")Var(",EQ2$env2 ,")+E(",EQ2$env2 ,")^2.Var(",EQ3$dev2 ,")$"),
                           paste0("$V_{",EQ3$dev12, EQ2$env12,"} = Var(",EQ3$dev12,")Var(",EQ2$env12,")+E(",EQ2$env12,")^2.Var(",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev1 , EQ2$env1, "}=E(",EQ2$env1 ,").Cov(",EQ3$dev0,",",EQ3$dev1 ,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev2 , EQ2$env2, "}=E(",EQ2$env2 ,").Cov(",EQ3$dev0,",",EQ3$dev2 ,")$"),
                           paste0("$COV_{",EQ3$dev0,",",EQ3$dev12, EQ2$env12,"}=E(",EQ2$env12,").Cov(",EQ3$dev0,",",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev1, EQ2$env1,",",EQ3$dev2, EQ2$env2,"}=E(",EQ2$env1,").E(",EQ2$env2,").Cov(",EQ3$dev1,",",EQ3$dev2,")$"),
                           paste0("$COV_{",EQ3$dev1, EQ2$env1,",",EQ3$dev12, EQ2$env12,"}=E(",EQ2$env1,").E(",EQ2$env12,").Cov(",EQ3$dev1,",",EQ3$dev12,")$"),
                           paste0("$COV_{",EQ3$dev2, EQ2$env2,",",EQ3$dev12, EQ2$env12,"}=E(",EQ2$env2,").E(",EQ2$env12,").Cov(",EQ3$dev2,",",EQ3$dev12,")$"),
                           paste0("$V_",NOT$groupV,"=Var(",NOT$groupV,")$"),
                           paste0("$V_",NOT$residualUpper,"=Var(",NOT$error,")$"),
                           paste0("$V_",NOT$total,"$")
  ),
  
  "Remarks"            = c("Remarks",
                           "",
                           "-",
                           paste0("In SQuID $Var(",EQ2$env1,")=1$."),
                           paste0("In SQuID $Var(",EQ2$env2,")=1$."),
                           paste0("Since in SQuID $Var(",EQ2$env1,")=Var(",EQ2$env2,")=1$ and $",EQ2$env1,"$ and $",EQ2$env2,"$ are independent of each other, the expected variance of the product is $Var(",EQ2$env12,")=1$. $^c$"),
                           "",
                           paste0("In the presence of random slope variation, $V_",EQ3$dev0,"$ expresses the variance at the point where all covariates are zero. Since all covariates are centred to zero in SQuID, this represents the variance at average values of the covariate(s)."),
                           paste0("In SQuID $Var(",EQ2$env1,")=1$ and $E(",EQ2$env1,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev1, EQ2$env1,"}=Var(",EQ3$dev1,")$."),
                           paste0("In SQuID $Var(",EQ2$env2,")=1$ and $E(",EQ2$env2,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev2, EQ2$env2,"}=Var(",EQ3$dev2,")$."),
                           paste0("In SQuID $Var(",EQ2$env1,")=Var(",EQ2$env2,")=1$, $E(",EQ2$env1,")=E(",EQ2$env2,")=0$, $",EQ2$env1,"$ and $",EQ2$env2,"$ are independent of each other, the expected variance of the product $Var(",EQ2$env12,")=1$ and the expected mean of the product is $E(",EQ2$env12,")=0$, which considerably simplifies the equation to $V_{",EQ3$dev12, EQ2$env12,"}=Var(",EQ3$dev12,")$. $^c$"),
                           paste0("In SQuID $E(",EQ2$env1,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           paste0("In SQuID $E(",EQ2$env2,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           paste0("In SQuID $E(",EQ2$env1,")=E(",EQ2$env2,")=0$ and an expected mean of the product of $E(",EQ2$env12,")=0$ and hence the covariance does not contribute to total phenotypic variance. $^d$"),
                           "-",
                           "-",
                           "-"
  )
  
 ), header=TRUE)

span(
  p(fullmodelTxt$summaryStepbyStep),
  div(class="line"),
  h4("Phenotypic equation"),
  source("./source/pages/full_model_sbys/ui_fullmodel_equation_2.R",local=TRUE)[["value"]],
  div(class="line"),
  h4(HTML("Summation of variance components<sup>a</sup>")),
  source("./source/pages/full_model_sbys/ui_fullmodel_variance_equation.R",local=TRUE)[["value"]],
  div(class="line"),
  myTable,
  div(class="line"),
  p(HTML("<sup>a</sup> Covariance parameters exist but do not contribute to total phenotypic variance.")),
  p(HTML(paste0("<sup>b</sup> Variances as they contribute to the total phenotypic variance.
         Note that we use $V_x$ and $Var(x)$ as alternative notations for the variances, $COV_{x,y}$ and $Cov(x,y)$ as alternative notations for covariances and $E(x)$ for expectations."))),
  p(HTML(paste0("<sup>c</sup> We anticipate that the covariance between $",EQ2$env1,"$ and $",EQ2$env2,"$ can be set by the user while SQuID evolves, which will affect $V_{",EQ2$env1,EQ2$env2,"}$ and $E(",EQ2$env1,EQ2$env2,")$."))),
  p(HTML(paste0("<sup>d</sup> Note the distinction between $COV_{",EQ3$dev0,",",EQ3$dev1, EQ2$env1,"}$ as a potential contributor to the variance and $Cov(",EQ3$dev0,",",EQ3$dev1,")$ as a covariance between intercepts and slopes, and that it can be simulated and estimated. 
                Mean centring of the environmental gradients has the advantage that we can interpret the intercept variance as the variance at an average environmental value and the intercept-slope covariance as the location of the minimum of the between-individual variance. 
                With arbitrary scaling of the environmental gradients, the interpretation of the intercept variance will change and $COV_{",EQ3$dev0,",",EQ3$dev1, EQ2$env1,"}$ will have to appear in the summation of variance components."))),
  p(HTML(paste0("<sup>e</sup> We use $V_",NOT$residualUpper,"$ to indicate $Var(",EQ1$error,")$ for two reasons. 
                First, $",EQ1$error,"$ is conventional notation for the deviation of an observation 
                from the values predicted by a statistical model and $V_",NOT$residualUpper,"$ is conventional 
                notation for the residual variance. In the SQuID modules, 
                we also introduce $V_",NOT$envEffect,"$, the variance in phenotype due to environment.  
                $V_",NOT$error,"$  and $V_",NOT$envEffect,"$ would mean very different things, 
                so to avoid confusion we adopt $V_",NOT$residualUpper,"$ to indicate residual variance.")))
)