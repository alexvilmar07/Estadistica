# 1. DATA TYPES AND DATA STORAGE IN R

# A. Qualitative Nominal Variable (Categorical without order), detects only the non-repeated values and orders them alphabetically
nominal_data <- c("CategoryA", "CategoryB", "CategoryA", "CategoryC")
nominal_factor <- factor(nominal_data)

# B. Qualitative Ordinal Variable (Categorical with order)
ordinal_data <- c("Low", "High", "Medium", "High", "Low")
ordinal_factor <- factor(ordinal_data, 
                         levels = c("Low", "Medium", "High"), 
                         ordered = TRUE)

# C. Quantitative Discrete Variable
discrete_data <- c(x, y, z)

# D. Quantitative Continuous Variable
continuous_data <- c(x, y, z)

# 2. FREQUENCY TABLES AND GRAPHICS (DISCRETE & QUALITATIVE DATA)

# Generic data vector: data <- c(x, y, z)
data <- c(x, y, z)

# Absolute frequencies (n_i)
n_i <- table(data)

# Relative frequencies (f_i)
f_i <- prop.table(n_i)

# Cumulative absolute frequencies (N_i)
N_i <- cumsum(n_i)

# Cumulative relative frequencies (F_i)
F_i <- cumsum(f_i)

# Full Statistical Table
freq_table <- data.frame(
  X = names(n_i),
  n = as.numeric(n_i),
  f = round(as.numeric(f_i), 2),
  N = as.numeric(N_i),
  F = round(as.numeric(F_i), 2)
)
print("Frequency Table:")
print(freq_table)

# Bar Chart and Pie Chart for Qualitative Data
data <- c(x, y, z)
names(data) <- c("Group1", "Group2", "Group3", "Group4")

# Barplot
barplot(data, main = "", col = "")

# Pie Chart
pie(data, main = "", col =  (length(data)))

# 3. GROUPED DATA (CONTINUOUS VARIABLES & HISTOGRAMS)

# Generic grouped data representation
lower_limits <- c(x, y, z)
upper_limits <- c(t, r, s))
n_grouped <- c(a, b, c)

# Class Marks (x_i) and Interval Widths (a_i)
class_marks <- (lower_limits + upper_limits) / 2
interval_width <- upper_limits - lower_limits

# Histogram for raw continuous data
data <- c(x, y, z)
hist(data, breaks = , col = "", main = "", xlab = "")

# 4. MEASURES OF CENTRAL TENDENCY AND POSITION

data <- c(x, y, z)

# Arithmetic Mean (\mu)
mean_val <- mean(data)

# Median (Me)
median_val <- median(data)

# Mode Function (Custom R function)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_val <- get_mode(data)

# Weighted Mean
values <- c(x, y, z)
weights <- c(t, r, s)
weighted_mean_val <- weighted.mean(values, weights)

# Quadratic Mean / Root Mean Square (RMS)
rms_val <- sqrt(mean(data^2))

# Quantiles, Quartiles, and Percentiles
quantiles <- quantile(data, probs = c(0.25, 0.50, 0.75))
percentiles <- quantile(data, probs = c(0.37, 0.68))


# 5. MEASURES OF DISPERSION AND COMPARISON

# Range
r_range <- max(data) - min(data)

# Interquartile Range (IQR)
iqr_val <- IQR(data)

# Population Variance (\sigma^2)
pop_var <- function(v) {
  mean((v - mean(v))^2)
}
var_pop <- pop_var(data)

# Population Standard Deviation (\sigma)
sd_pop <- sd(data)

# Sample Quasi-variance (s^2)
sample_quasivar <- var(data)

# Sample Quasi-standard deviation (s)
sample_quasisd <- sd(data)

# Coefficient of Variation (CV)
cv_val <- (sd_pop / abs(mean_val)) * 100

# Standardization / Z-scores
val_to_standardize <- n
z_score <- (val_to_standardize - mean_val) / sd_pop

# 6. MOMENTS AND SHAPE MEASURES (SKEWNESS & KURTOSIS)

# Ordinary Moment of order r (m_r)
m_r <- function(v, r) {
  mean(v^r)
}

# Central Moment of order r (\mu_r)
mu_r <- function(v, r) {
  mean((v - mean(v))^r)
}

# Sample dataset for moments
dataset <- c(x, y, z)

mu3 <- mu_r(dataset, 3)
mu4 <- mu_r(dataset, 4)
sigma_ds <- sqrt(pop_var(dataset))

# Fisher's Skewness Coefficient (g1)
g1 <- mu3 / (sigma_ds^3)

# Fisher's Kurtosis Coefficient (g2)
g2 <- (mu4 / (sigma_ds^4)) - 3

print(paste("Fisher Skewness (g1):", round(g1, 4)))
print(paste("Fisher Kurtosis (g2):", round(g2, 4)))