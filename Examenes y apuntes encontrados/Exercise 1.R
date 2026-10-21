#Exercise 1
library(tidyverse)
stats::filter

#a) Covariance
getRawBivariateStats <- function(x, y){
  
  n <- length(x)
  mean_X <- mean(x)
  mean_Y <- mean(y)
  
  variance_X <- sum((x - mean_X)^2) / n
  variance_Y <- sum((y - mean_Y)^2) / n
  covariance_XY <- sum((x - mean_X) * (y - mean_Y)) / n
}
x <- c(10,20,30,40,50,60,70,80)
y <- c(52,61,75,93,118,150,186,240)

print(getRawBivariateStats(x,y))

#b) Correlation coefficient
print(cor(x, y, method = "pearson"))


model_yx <- lm(y ~ x)
print(coef(model_yx))


model_xy <- lm(x ~ y)
print(coef(model_xy))

#c) Exponential fit

model_exp <- lm(log(y) ~ x)
a <- exp(coef(model_exp)[1])
b <- coef(model_exp)[2]
print(c(a = a, b = b))

print(summary(lm(y ~ x))$r.squared)

#e) Parabola

model_parabola <- lm(y ~ x + I(x^2))
print(coef(model_parabola))
print(summary(lm(y ~ x))$r.squared)