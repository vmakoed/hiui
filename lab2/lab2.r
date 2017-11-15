analyse_regression <- function(x, y) {
  model <- lm(y ~ x)
  print(summary(model))
  plot(x, y)
  abline(model)
}

dat <- read.table("~/dev/hiui/16-housing.txt", header=TRUE)

cat("statData")
analyse_regression(dat$CRIM, dat$INDUS)

n <- 100
a <- -0.1
b <- -0.1
s2 <- 0.01
x <- seq(0.0, 1.0, length=n)
y <- a * x + b + rnorm(n, 0, s2)

cat("randData")
analyse_regression(x, y)
