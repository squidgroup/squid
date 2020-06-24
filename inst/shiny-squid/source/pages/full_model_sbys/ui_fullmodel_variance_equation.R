p(
  paste0("$$V_", NOT$total  , "=
         V_{" , EQ3$mean1 , EQ2$env1, "} + V_{" , EQ3$mean2 , EQ2$env2, "} + V_{" , EQ3$mean12, EQ2$env1, EQ2$env2, "} +
         V_{" , EQ3$dev0  , "} +
         V_{" , EQ3$dev1  , EQ2$env1, "} + V_{" , EQ3$dev2  , EQ2$env2, "} + V_{" , EQ3$dev12, EQ2$env1, EQ2$env2, "} +", 
         "V_" , NOT$groupV, "+", 
         "V_" , NOT$residualUpper ,
         "$$")
)
