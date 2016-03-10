p(
  paste0("$$", NOT$trait.1  , "=",
         EQ3$mean0 , "+", EQ3$mean1,EQ2$env1, "+", EQ3$mean2,EQ2$env2, "+", EQ3$mean12,EQ2$env12, "+", 
         EQ3$dev0  , "+", EQ3$dev1,EQ2$env1 , "+", EQ3$dev2,EQ2$env2 , "+", EQ3$dev12,EQ2$env12 , "+", 
         NOT$groupV, "+", 
         NOT$error,
         "$$")
)
