library(MASS)
set.seed(1839)

R1 <- 0.40
R0 <- 0.00
cond1.cor <- matrix(c(1, R1, R1, 1), nrow = 2, ncol = 2, 
                    dimnames = list(c("prej", "auth"), c("prej", "auth")))
cond0.cor <- matrix(c(1, R0, R0, 1), nrow = 2, ncol = 2, 
                    dimnames = list(c("prej", "auth"), c("prej", "auth")))
Ns <- seq(100, 300, 20)
results <- data.frame(N = Ns, power = NA)

for (i in Ns) {
  p <- c()
  for (j in 1:1000) {
    dat1 <- as.data.frame(
      mvrnorm(n = round(i / 2, 0), mu = c(prej = 0, auth = 0), Sigma = cond1.cor)
    )
    dat1$cond <- "cond0"
    dat0 <- as.data.frame(
      mvrnorm(n = round(i / 2, 0), mu = c(prej = 0, auth = 0), Sigma = cond0.cor)
    )
    dat0$cond <- "cond1"
    dat <- rbind(dat1, dat0)
    p[j] <- summary(lm(auth ~ prej * cond, dat))$coef[4, 4] <= .05
  }
  results[results$N == i, "power"] <- mean(p)
}

results
