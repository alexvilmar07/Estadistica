x <- c(120,135,128,142,131,125,138,133,129,410,127,136,140,132,130,126,134,137,131,145)

print(mean(x))

print(median(x))

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(mode_val <- get_mode(x))

print(quantiles <- quantile(x, probs = c(0.25, 0.50, 0.75)))

print(iqr_val <- IQR(x))

pop_var <- function(v) {
  mean((v - mean(v))^2)
}
print(var_pop <- pop_var(x))