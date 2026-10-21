x <- c(10,20,30,40,50,60,70,80)
y <- c(52,61,75,93,118,150,186,240)

mean_x <- mean(x)
mean_y <- mean(y)
m11 <- mean(x*y)
cov_xy <- m11 - mean_x*mean_y

cat("\nCOVARIANCE:",cov_xy)

var_X <- mean((x-mean(x))^2)
var_Y <- mean((y-mean(y))^2)
r <- cov_xy/(sqrt(var_X)*sqrt(var_Y))

cat("\n r:", r)

b <- cov_xy/var_X

a <- mean_y-b*mean_x

cat("\n a = ", a)
cat("\n b = ", b)

bX <- cov_xy/var_Y

aX <-  mean_x-bX*mean_y

cat("\n aX = ", aX)
cat("\n bX = ", bX)


Y = log(y)

mean_Y <- mean(Y)
m11 <- mean(x*Y)
cov_xY <- m11 - mean_x*mean_Y

cat("\nCOVARIANCE:",cov_xy)

var_X <- mean((x-mean(x))^2)
var_Y <- mean((Y-mean(Y))^2)

b <- cov_xY/var_X

a <- mean_Y-b*mean_x
exp_a <- exp(a)

cat("\nexp a:",exp_a )
cat("\nb:",b )

R2 <- 1-MSE/var_y_original

MSE 