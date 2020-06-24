
myTable <- getTable(data.frame(
  
  "Component"          = c("Variance component",
                           "$\\text{Fixed effects}$",
                           paste0("$COV_{",NOT$mean,"_{",NOT$trait.1,"}, ",NOT$mean,"_{",NOT$trait.2,"}}$"),
                           paste0("$COV_{",NOT$mean,"_{1",NOT$trait.1,"}",NOT$mean,"_{2",NOT$trait.1,"}, ",NOT$mean,"_{1",NOT$trait.2,"}",NOT$mean,"_{2",NOT$trait.2,"}}$"),
                           "$\\text{Random effects}$",
                           paste0("$COV_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$devS,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$devS,"_",NOT$trait.1,",",NOT$devS,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$devS,"_{1",NOT$trait.1,"},",NOT$devS,"_{2",NOT$trait.2,"}}$"),
                           paste0("$COV_{",NOT$devS,"_{12",NOT$trait.1,"},",NOT$devI,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$devS,"_{12",NOT$trait.1,"},",NOT$devS,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$devS,"_{12",NOT$trait.1,"},",NOT$devS,"_{12",NOT$trait.2,"}}$"),
                           paste0("$COV_{",NOT$groupV,"_",NOT$trait.1,",",NOT$groupV,"_",NOT$trait.2,"}$"),
                           paste0("$COV_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$")
  ),
  
  "Explanation"        = c("Explanation",
                           "",
                           paste0("Population-average (i.e., within-individual) covariance between the reaction norm 
                                  slope for traits $",NOT$trait.1,"$ and $",NOT$trait.2,"$ (i.e., linear within-individual response to environmental 
                                  effect $",NOT$env,"$); exists for all (maximum two) fitted effects $(",EQ2$env1,", ",EQ2$env2,")$."),
                           paste0("Population-average (i.e., within-individual) covariance between a reaction norm 
                                  slope interaction between two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$ 
                                  for trait y and the same slope interaction for trait $",NOT$trait.2,"$."),
                           "",
                           paste0("Individual-specific covariance between reaction norm intercepts for traits $",NOT$trait.1,"$ and $",NOT$trait.2,"$."),
                           paste0("Individual-specific covariance between a reaction norm slope for trait $",NOT$trait.1,"$ 
                                  (i.e., linear within-individual response to environmental effect $",NOT$env,"$) 
                                  and an intercept for trait $",NOT$trait.2,"$; exists for all (maximum two) fitted effects$(",EQ2$env1,", ",EQ2$env2,")$; 
                                  exists also for the reverse scenario (i.e., $COV_{",NOT$devI,"_",NOT$trait.1,",",NOT$devS,"_",NOT$trait.2,"}$)."),
                           paste0("Individual-specific covariance between a reaction norm slope for trait $",NOT$trait.1,"$ 
                                  (i.e., linear within-individual response to environmental effect $",NOT$env,"$) 
                                  and the same slope for trait z; exists for up to two effects $(",EQ2$env1,", ",EQ2$env2,")$  
                                  (i.e., $COV_{",NOT$devS,"_{1",NOT$trait.1,"},",NOT$devS,"_{1",NOT$trait.2,"}}$, 
                                  $COV_{",NOT$devS,"_{2",NOT$trait.1,"},",NOT$devS,"_{2",NOT$trait.2,"}}$)"),
                           paste0("Individual-specific covariance between a reaction norm slope for trait $",NOT$trait.1,"$ 
                                  (i.e., linear within-individual response to environmental effect $",EQ2$env1,"$) 
                                  and another slope for trait $",NOT$trait.2,"$ (i.e., response to effect $",EQ2$env2,"$); 
                                  exists also for reverse scenario (i.e., $COV_{",NOT$devS,"_{2",NOT$trait.1,"},",NOT$devS,"_{1",NOT$trait.2,"}}$)"),
                           paste0("Individual-specific covariance between a reaction norm slope interaction between 
                                  two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$ for trait $",NOT$trait.1,"$ and an intercept for trait $",NOT$trait.2,"$; 
                                  exists also for the reverse scenario (i.e., $COV_{",NOT$devI,"_",NOT$trait.1,",",NOT$devS,"_{12",NOT$trait.2,"}}$)"),
                           paste0("Individual-specific covariance between a reaction norm slope interaction between 
                                  two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$ for trait $",NOT$trait.1,"$ and a slope for trait $",NOT$trait.2,"$ 
                                  (i.e., linear within-individual response to environmental effect $",EQ2$env1,"$ or  $",EQ2$env2,"$), 
                                  resulting in, respectively, $COV_{",NOT$devS,"_{12",NOT$trait.1,"},",NOT$devS,"_{1",NOT$trait.2,"}}$ or 
                                  $COV_{",NOT$devS,"_{12",NOT$trait.1,"},",NOT$devS,"_{2",NOT$trait.2,"}}$; 
                                  exists also for reverse scenarios ($COV_{",NOT$devS,"_{1",NOT$trait.1,"},",NOT$devS,"_{12",NOT$trait.2,"}}$, 
                                  $COV_{",NOT$devS,"_{2",NOT$trait.1,"},",NOT$devS,"_{12",NOT$trait.2,"}}$)."),
                           paste0("Individual-specific covariance between a reaction norm slope interaction between 
                                  two environmental effects $(",EQ2$env1,", ",EQ2$env2,")$ for trait $",NOT$trait.1,"$ and the same 
                                  slope interaction for trait $",NOT$trait.2,"$."),
                           paste0("Higher-order covariance between reaction norm intercepts for traits $",NOT$trait.1,"$ and $",NOT$trait.2,"$."),
                           paste0("Residual covariance between reaction norm intercepts for traits $",NOT$trait.1,"$ and $",NOT$trait.2,"$.")

  )
 ), header=TRUE)

span(
  p(HTML(fullmodelTxt$bivariateStepbyStep)),
  div(class="line"),
  myTable
)








