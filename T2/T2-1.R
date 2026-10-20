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