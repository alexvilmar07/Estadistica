# Plotting a seasonal time series -----------------------------------------
# Use this library
library(tidyverse)

# For a sequence X {x1, x2, x3, x4..., xk}
xx <- c(x1, x2, x3, ..., xk)

# Reading the data
# Indicate the frequency of the seasonal component
firstyear <- # first year duh
  freq <- # -_-
  
  uk <- ts(xx, start = c(firstyear, 1), # we want the first quarter
           frequency = freq # Quarterly? freq = 4
  )
plot(uk)

# Moving average of order n (impar)
TC <- stats::filter(uk, c(1, 1, 1, ..., 1) / k) 
# (We put as many ones as elements in X)  

# Moving average of order n (par)
TC <- stats::filter(uk, c(0.5, 1, 1, ..., 0.5) / k)
# (We put k+1 elements, with the extremes being 0.5, the rest 1)

# lines(tend, col = "purple")

est_aleM <- uk / TC

# print("est_aleM:")
# print(est_aleM)

# Unnormalized Seasonal Component
est1M <- colMeans(matrix(est_aleM, ncol = freq, byrow = TRUE), na.rm = TRUE)

# Normalized
estM <- est1M / mean(est1M)
# print("estM:")
# print(estM)

# Random Component
aleM <- est_aleM / estM
# print("aleM:")
# print(aleM)


# Deseasonalisation -------------------------------------------------------

desestM <- uk / estM
# print("Desest M: ")
# print(desestM)
plot(uk)
lines(desestM, col = 4)


# Regression line ---------------------------------------------------------

xx <- seq_along(uk)

model <- lm(uk ~ xx)

regL <- ts(predict.lm(model), start = c(firstyear, 1), frequency = freq)

print("Intercept line:")
intercept <- round(model$coefficients[1], 5)
xslope <- round(model$coefficients[2], 5)
print(paste(intercept, " ", xslope, "x"))

plot(uk)
lines(regL, col = 2)


# Function that calculates autocorrelation for a frequency n ---------------

data <- c(# datapoints
)

autoCorrel <- function(data, n) {
  # Remove the last n data points
  xx1 <- head(data, -n)
  # Remove the first n data points
  xx2 <- tail(data, -n)
  # correlation
  sqrt(summary(lm(xx1 ~ xx2))$r.squared)
}

# The frequency searched will be the one that has the highest correlation.
# For that, we check which one the highest is between 1 and 12 and that will be the answer.

data <- c(# data points
)

highest <- 0
freq <- 1
top <- 12

if (length(data) < 12) {
  top <- length(data) - 1
}

for (i in 1:top) {
  current <- autoCorrel(data, i)
  
  if (current > highest) {
    highest <- current
    freq <- i
  }
}

print(paste("The frequency will be: ", freq, ", with correlation: ", round(highest, 5)))