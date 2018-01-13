set.seed(1839)
n <- 200
reps <- 1000
ds <- seq(.3, .5, .01)
rs <- c()
for (i in ds) {
  tmp <- c()
  for (j in 1:reps) {
    x <- c(rep(0, n / 2), rep(1, n / 2))
    y <- c(rnorm(n / 2), rnorm(n / 2, i, 1))
    tmp[j] <- cor(x, y)
  }
  rs[which(ds == i)] <- mean(tmp)
}
data.frame(rs, ds)
# about d = .45 matches the r = .22 observed in study 3
