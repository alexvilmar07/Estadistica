################################################################################
# A) MOVING AVERAGES - SLIDES EXAMPLE
################################################################################
x <- c(120.0000,95.0000,80.0000,140,132,104,90,153,143,115,97,166,155,124,106,180)


X <- ts(x, start=c(2020,1), frequency=4)

TC <- stats::filter(X, c(.5,1,1,1,.5)/4)

cat("\nTC:\n"); print(TC)

EA <- X/TC

EA_matrix <- matrix(EA, ncol=4, byrow=TRUE)
E_uncorrected <- colMeans(EA_matrix, na.rm=TRUE)
names(E_uncorrected) <- c("Spring","Summer","Autumn","Winter")

E <- E_uncorrected/mean(E_uncorrected)
A <- EA/E

X_deseasonalised <- X/E

t <- seq_along(X)
trend_model <- lm(X_deseasonalised ~ t)
T_values <- predict(trend_model)
T <- ts(T_values, start=c(2020,1), frequency=4)
b <- coef(trend_model)

cat("\nTC:\n"); print(TC)
cat("\nEA:\n"); print(EA)
cat("\nE uncorrected:\n"); print(E_uncorrected)
cat("\nE corrected:\n"); print(E)
cat("\nA:\n"); print(A)
cat("\nDeseasonalised X/E:\n"); print(X_deseasonalised)
cat("\nTrend model:\n"); print(trend_model)
cat("\nSlop of T, (b):\n"); print(b)
cat("\nCorrected Trend T:\n"); print(T)


t_future <- 17:18
T_future <- coef(trend_model)[1] + coef(trend_model)[2]*t_future
X_hat <- T_future * E          # if E is Spring,Summer,Autumn,Winter

cat("\nX_HAT T:\n"); print(T_future)

k <- 4

x1 <- head(x, -k)   # removes last k values
x2 <- tail(x, -k)   # removes first k values
r <- cor(x1, x2)
cat("\nR: ", r)