x <- c(120,135,128,142,131,125,138,133,129,410,127,136,136,140,132,130,126,134,137,131,145)
print(length(x))
N <- length(x)

meanX <- mean(x)
mu <- mean(x)

cat("\nMEAN:",meanX)

medianX <- median(x)

cat("\nMEADIAN:",medianX)



#moda <- as.numeric(names(x)[x == max(x)])
#print(moda)


Q1 <- quantile(x, .2500, type=2)
Q3 <- quantile(x, .7500, type=2)

IQR <- Q3-Q1

cat("\nQ1: ", Q1)
cat("\nQ2: ", Q3)
cat("\nIQR: ", IQR)


sample_variance_n <- mean((x-mean(x))^2)

cat("\nVARIANCE:", sample_variance_n)

sigma <- sqrt(sample_variance_n)

CV <- sigma/abs(mean(x))

cat("\nCV:",CV)


mu3 <- mean((x-mean(x))^3)
cat("\nMU3:",mu3)
g1 <- mu3/sigma^3
cat("\ng1:",g1)

mu4 <- mu4 <- mean((x-mu)^4)
cat("\nMU4:",mu4)
g2 <- mu4/sigma^4-3
cat("\ng2:",g2)


AP <- (mu-Mo)/sigma