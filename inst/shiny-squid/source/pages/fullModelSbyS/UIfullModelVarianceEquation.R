p(
  paste0("$$V_", NOT$total  , "=
         V_{" , EQ3$mean1 , "} + V_{" , EQ3$mean2 , "} + V_{" , EQ3$mean12, "} +
         V_{" , EQ3$dev0  , "} +
         V_{" , EQ3$dev1  , "} + V_{" , EQ3$dev2  , "} + V_{" , EQ3$dev12, "} +", 
         "V_" , NOT$groupV, "+", 
         "V_" , NOT$error ,
         "$$")
)
