#Exercise 2

library(tidyverse)

xx <- c (120,95,80,140,132,104,90,153,143,115,97,166,155,124,106,180)



#Moving average of order n (even)

uk <- ts(xx, start=c(2020,1), frequency=4)

TC <- stats::filter(uk, c(0.5,1,1,1,0.5)/4)

cat("\nTC:\n"); print(TC)
est_aleM <- uk / TC
print("est_aleM: " )
print(est_aleM)

print(est1M <- colMeans (matrix ( est_aleM , ncol =freq , byrow = T ) ,
                    na.rm = T))
#Corrected seasonal index
estM <- est1M / mean ( est1M )
 print("estM:")
 print(estM)

#Random component
 
 aleM <- est_aleM / estM
 print("aleM:")
 print(aleM)
 
 #Deseasonalized series...
 
 desestM <- uk / estM
 print("Desest M: ")
 print(desestM)
 
 t <- seq_along(uk)
 trend_model <- lm(desestM ~ t)
 T_values <- predict(trend_model)
 T <- ts(T_values, start=c(2020,1), frequency=4)
 print(b <- coef(trend_model))
 
 