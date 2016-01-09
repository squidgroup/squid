p(

  paste0("$$", EQ$phen.1  , "=",
        "(" , EQ$mean0.1 , "+", EQ$dev0.1 , ")+",
        "(" , EQ$mean1.1 , "+", EQ$dev1.1 , ")" , EQ$env1 , "+",
        "(" , EQ$mean2.1 , "+", EQ$dev2.1 , ")" , EQ$env2 , "+",
        "(" , EQ$mean12.1, "+", EQ$dev12.1, ")" , EQ$env12, "+",
        EQ$group.1, "+", 
        EQ$error.1,
        "$$"),
  
  paste0("$$", EQ$phen.2  , "=",
        "(" , EQ$mean0.2 , "+", EQ$dev0.2 , ")+",
        "(" , EQ$mean1.2 , "+", EQ$dev1.2 , ")" , EQ$env1 , "+",
        "(" , EQ$mean2.2 , "+", EQ$dev2.2 , ")" , EQ$env2 , "+",
        "(" , EQ$mean12.2, "+", EQ$dev12.2, ")" , EQ$env12, "+",
        EQ$group.2, "+", 
        EQ$error.2,
        "$$", sep="")
)
