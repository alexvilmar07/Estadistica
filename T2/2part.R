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
