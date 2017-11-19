require(class)
require(MASS)
require(rgl)

plot_points <- function(train, test, clazz.train,
                        clazz.test) {
  rgl.open()
  rgl.bg(color=c("white", "white"))
  plot3d(train[, 1], train[, 2], train[, 3],
         col=clazz.train, type='p', size=5, add=FALSE)
  plot3d(test[, 1], test[, 2], test[, 3],
         col=clazz.test, type='s', size=1, add=TRUE)
}

analyse_knn <- function(dat, clazz) {
  n <- nrow(dat)
  rnd.num <- sample(1 : n)
  train.num <- rnd.num[1 : (n %/% 2)]
  test.num <- rnd.num[(n %/% 2 + 1) : n]
  train <- dat[train.num,]
  test <- dat[test.num,]
  clazz.train <- clazz[train.num]
  clazz.test <- clazz[test.num]
  clazz.knn <- knn(train, test, clazz.train)
  print(sum(clazz.test != clazz.knn) / n)
  plot_points(train, test, clazz.train, clazz.test)
  plot_points(train, test, clazz.train, clazz.knn)
}

dat <- read.table("~/dev/hiui/16-housing.txt", header=TRUE)

cat("statData")
analyse_knn(cbind(dat$CRIM, dat$INDUS,
                  dat$NOX), unclass(dat$AGE))
n1 <- 1000
a1 <- c(3, 5, 7)
r1 <- cbind(c(4, 1, 1), c(1, 2, 0.1), c(1, 0.1, 2))
n2 <- 50
a2 <- c(7, 1, 1)
r2 <- cbind(c(3, -1, 1), c(-1, 3, 1), c(1, 1, 2))
dat <- rbind(mvrnorm(n1, a1, r1), mvrnorm(n2, a2, r2))

cat("randData")
analyse_knn(dat, c(rep(1, n1), rep(2, n2)))

