p(
  paste0("$$", EQ1$phen  , "=",
  			 EQ1$mean0 , "+", EQ1$mean1,EQ1$env1, "+", EQ1$mean2,EQ1$env2, "+", EQ1$mean12,EQ1$env12, "+", 
  			 EQ1$dev0  , "+", EQ1$dev1,EQ1$env1 , "+", EQ1$dev2,EQ1$env2 , "+", EQ1$dev12,EQ1$env12 , "+", 
  			 EQ1$group, "+", 
  			 EQ1$error,
         "$$")
)
