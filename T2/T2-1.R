# PART 1: JOINT DESCRIPTION OF SEVERAL VARIABLES

# Creates the joint absolute and relative frequency tables from two vectors of raw data.
pairToTable <- function(x, y){
  
  absoluteTable <- table(x, y)
  
  # Uncomment if you need relative frequencies
  # relativeTable <- prop.table(absoluteTable)
}

# Builds a two-dimensional frequency table when data is already given as a matrix.
createTwoDimensionalTable <- function(data_by_row, x_categories, y_categories){
  
  absoluteTable <- matrix(data_by_row, nrow = length(x_categories), byrow = TRUE, dimnames = list(x_categories, y_categories))
  
  # Uncomment if you need relative frequencies
  # relativeTable <- prop.table(absoluteTable)
}

# Calculates class marks and interval widths when data is grouped in intervals.
getClassMarks <- function(lower_limits, upper_limits){
  
  classMarks <- (lower_limits + upper_limits) / 2
  
  # Uncomment if you need interval widths
  # intervalWidths <- upper_limits - lower_limits
}

# Computes the marginal distributions of X (rows) and Y (columns) from a joint table.
getMarginals <- function(twoDimensionalTable){
  
  marginalAbsolute_X <- rowSums(twoDimensionalTable)
  marginalAbsolute_Y <- colSums(twoDimensionalTable)
  
  # Uncomment the one you want to return
  # marginalRelative_X <- prop.table(marginalAbsolute_X)
  # marginalRelative_Y <- prop.table(marginalAbsolute_Y)
}

# Extracts the conditional distribution of Y given a specific value of X (row).
getConditional_Y_given_X <- function(twoDimensionalTable, x_value){
  
  conditionalAbsolute_Y <- twoDimensionalTable[as.character(x_value), ]
  
  # Uncomment if you need relative frequencies
  # conditionalRelative_Y <- prop.table(conditionalAbsolute_Y)
}

# Extracts the conditional distribution of X given a specific value of Y (column).
getConditional_X_given_Y <- function(twoDimensionalTable, y_value){
  
  conditionalAbsolute_X <- twoDimensionalTable[, as.character(y_value)]
  
  # Uncomment if you need relative frequencies
  # conditionalRelative_X <- prop.table(conditionalAbsolute_X)
}

# Calculates mean, variance, median, and mode for any 1D distribution (marginal or conditional).
getDistributionStats <- function(categories, frequencies){
  
  relativeFreq <- prop.table(frequencies)
  cumulativeRelFreq <- cumsum(relativeFreq)
  meanValue <- sum(categories * relativeFreq)
  
  # Uncomment the specific measure you need
  # varianceValue <- sum((categories^2) * relativeFreq) - (meanValue^2)
  # medianValue <- categories[which(cumulativeRelFreq >= 0.5)[1]]
  # modeValue <- categories[which(relativeFreq == max(relativeFreq))]
}

# Calculates means (center of gravity), variances, and covariance from raw data vectors.
getRawBivariateStats <- function(x, y){
  
  n <- length(x)
  mean_X <- mean(x)
  mean_Y <- mean(y)
  
  variance_X <- sum((x - mean_X)^2) / n
  variance_Y <- sum((y - mean_Y)^2) / n
  covariance_XY <- sum((x - mean_X) * (y - mean_Y)) / n
}

# Calculates means, ordinary moments, variances, and covariance from a two-dimensional table.
getTableBivariateStats <- function(twoDimensionalTable, x_values, y_values){
  
  relativeTable <- prop.table(twoDimensionalTable)
  marginalRel_X <- rowSums(relativeTable)
  marginalRel_Y <- colSums(relativeTable)
  
  mean_X <- sum(x_values * marginalRel_X)
  mean_Y <- sum(y_values * marginalRel_Y)
  
  moment_20 <- sum((x_values^2) * marginalRel_X)
  moment_02 <- sum((y_values^2) * marginalRel_Y)
  moment_11 <- sum(outer(x_values, y_values, "*") * relativeTable)
  
  variance_X <- moment_20 - (mean_X^2)
  variance_Y <- moment_02 - (mean_Y^2)
  covariance_XY <- moment_11 - (mean_X * mean_Y)
}

# Computes the moment of order (r, s) around point (a, b) from raw data vectors.
getRawMoment <- function(x, y, r, s, a = 0, b = 0){
  
  n <- length(x)
  moment_rs <- sum(((x - a)^r) * ((y - b)^s)) / n
}

# Computes the moment of order (r, s) around point (a, b) from a two-dimensional table.
getTableMoment <- function(twoDimensionalTable, x_values, y_values, r, s, a = 0, b = 0){
  
  relativeTable <- prop.table(twoDimensionalTable)
  moment_rs <- sum(outer((x_values - a)^r, (y_values - b)^s, "*") * relativeTable)
}


# PART 2: DEPENDENCY AND INDEPENDENCE BETWEEN VARIABLES 

# Checks if X and Y are statistically independent by comparing joint and marginal frequencies.
checkIndependence <- function(twoDimensionalTable){
  
  relativeTable <- prop.table(twoDimensionalTable)
  expectedTable <- outer(rowSums(relativeTable), colSums(relativeTable), "*")
  
  isIndependent <- all(abs(relativeTable - expectedTable) < 1e-6)
}

# Checks if X functionally depends on Y (one non-zero cell per column) or Y on X (per row).
checkFunctionalDependency <- function(twoDimensionalTable){
  
  X_depends_on_Y <- all(colSums(twoDimensionalTable > 0) == 1)
  
  # Uncomment to check if Y functionally depends on X instead
  # Y_depends_on_X <- all(rowSums(twoDimensionalTable > 0) == 1)
}


# PART 4: MODEL FORMULATION (LEAST SQUARES METHOD) 

# Computes residuals, Sum of Squared Errors (SSE), and Mean Squared Error (MSE).
getLeastSquaresErrors <- function(y_observed, y_estimated){
  
  n <- length(y_observed)
  residuals_e <- y_observed - y_estimated
  sse <- sum(residuals_e^2)
  
  # Uncomment if you need the Mean Squared Error (MSE) instead of SSE
  # mse <- sse / n
}

# Computes the general regression curve of Y on X (conditional means of Y for each X).
getGeneralRegressionCurve <- function(twoDimensionalTable, y_values){
  
  conditionalRelByRow <- prop.table(twoDimensionalTable, margin = 1)
  conditionalMeans_Y_given_X <- as.vector(conditionalRelByRow %*% y_values)
}

# STATISTICAL METHODS — QUICK TEMPLATE
# Change x, y, z and the example values to match the exercise.

x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 7)

# Calculate the mean
mean(x)

# Calculate the median
median(x)

# Calculate the mode
mode_function <- function(x) { t <- table(x); as.numeric(names(t)[t == max(t)]) }
mode_function(x)

# Calculate the variance using divisor n
mean((x - mean(x))^2)

# Calculate the standard deviation using divisor n
sqrt(mean((x - mean(x))^2))

# Calculate the sample variance (divisor n-1)
var(x)

# Calculate the sample standard deviation
sd(x)

# Calculate the absolute frequencies
table(x)

# Calculate the relative frequencies
prop.table(table(x))

# Calculate the cumulative frequencies
cumsum(table(x))

# Create a frequency table from values and frequencies
values <- c(1, 2, 3, 4)
frequencies <- c(2, 5, 3, 1)
data.frame(value = values, frequency = frequencies,
           relative_frequency = frequencies / sum(frequencies),
           cumulative_frequency = cumsum(frequencies),
           cumulative_relative_frequency = cumsum(frequencies) / sum(frequencies))

# Calculate the mean from a frequency table
sum(values * frequencies) / sum(frequencies)

# Calculate the median from a frequency table
median(rep(values, frequencies))

# Calculate the mode from a frequency table
values[frequencies == max(frequencies)]

# Create a joint frequency table
table(x, y)

# Create a joint frequency table with marginal totals
addmargins(table(x, y))

# Calculate the marginal frequencies of X
margin.table(table(x, y), 1)

# Calculate the marginal frequencies of Y
margin.table(table(x, y), 2)

# Calculate the distribution of Y given X = 2
table(y[x == 2])

# Calculate the relative distribution of Y given X = 2
prop.table(table(y[x == 2]))

# Calculate the distribution of X given Y = 4
table(x[y == 4])

# Calculate the relative distribution of X given Y = 4
prop.table(table(x[y == 4]))

# Calculate the mean of Y given X = 2
mean(y[x == 2])

# Calculate the mean of X given Y = 4
mean(x[y == 4])

# Calculate the covariance using divisor n
mean((x - mean(x)) * (y - mean(y)))

# Calculate the sample covariance (divisor n-1)
cov(x, y)

# Calculate Pearson's correlation coefficient
cor(x, y, method = "pearson")

# Calculate Spearman's rank correlation coefficient
cor(x, y, method = "spearman")

# Draw a scatter plot
plot(x, y, pch = 19, xlab = "X", ylab = "Y")

# Calculate the regression line of Y on X
model_yx <- lm(y ~ x)
coef(model_yx)

# Draw the regression line of Y on X
plot(x, y, pch = 19)
abline(lm(y ~ x), col = "red", lwd = 2)

# Calculate the regression line of X on Y
model_xy <- lm(x ~ y)
coef(model_xy)

# Predict Y when X = 6
predict(lm(y ~ x), newdata = data.frame(x = 6))

# Predict X when Y = 6
predict(lm(x ~ y), newdata = data.frame(y = 6))

# Calculate the fitted values of Y
fitted(lm(y ~ x))

# Calculate the regression residuals
residuals(lm(y ~ x))

# Calculate the sum of squared errors (SSE)
sum(residuals(lm(y ~ x))^2)

# Calculate the coefficient of determination (R-squared)
summary(lm(y ~ x))$r.squared

# Calculate the residual variance using divisor n
mean(residuals(lm(y ~ x))^2)

# Fit a quadratic regression model
model_parabola <- lm(y ~ x + I(x^2))
coef(model_parabola)

# Predict using the quadratic regression model when X = 6
predict(model_parabola, newdata = data.frame(x = 6))

# Fit a logarithmic regression model: y = a + b*ln(x)
model_log <- lm(y ~ log(x))
coef(model_log)

# Predict using the logarithmic regression model when X = 6
predict(model_log, newdata = data.frame(x = 6))

# Fit an exponential regression model: y = a*exp(b*x) (requires y > 0)
model_exp <- lm(log(y) ~ x)
a <- exp(coef(model_exp)[1])
b <- coef(model_exp)[2]
c(a = a, b = b)

# Predict using the exponential regression model when X = 6
exp(predict(model_exp, newdata = data.frame(x = 6)))

# Fit a power regression model: y = a*x^b (requires x > 0 and y > 0)
model_power <- lm(log(y) ~ log(x))
a <- exp(coef(model_power)[1])
b <- coef(model_power)[2]
c(a = a, b = b)

# Predict using the power regression model when X = 6
exp(predict(model_power, newdata = data.frame(x = 6)))

# Test independence using the chi-square test
chisq.test(table(x, y), correct = FALSE)

# Calculate the contingency coefficient
tab <- table(x, y)
chi <- chisq.test(tab, correct = FALSE)$statistic
sqrt(as.numeric(chi) / (sum(tab) + as.numeric(chi)))

# Fit the plane z = a + b*x + c*y
z <- c(3, 5, 6, 8, 9)
model_plane <- lm(z ~ x + y)
coef(model_plane)

# Calculate the fitted values of the plane
fitted(model_plane)

# Calculate the sum of squared residuals for the plane
sum(residuals(model_plane)^2)

# Calculate the residual variance of the plane using divisor n
mean(residuals(model_plane)^2)

# Calculate the R-squared of the plane
summary(model_plane)$r.squared
