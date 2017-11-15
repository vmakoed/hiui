require(MASS)

analyse_cor <- function(x, y) {
  print(cor.test(x, y))
  plot(x, y)
}

dat <- read.table("~/dev/hiui/16-housing.txt", header=TRUE)

cat("statData")
analyse_cor(dat$CRIM, dat$INDUS)

n <- 100
a <- c(1, 1)
r <- cbind(c(1, -1), c(-1, 10))
dat <- mvrnorm(n, a, r)

cat("randData")
analyse_cor(dat[,1], dat[,2])
