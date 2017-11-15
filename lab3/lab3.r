require(MASS)

analyse_clust <- function(x, y, clazz) {
  k <- length(unique(clazz))
  clust <- kmeans(cbind(x, y), k)
  print(clust$totss)
  plot(x, y, col=as.factor(clazz))
  plot(x, y, col=as.factor(clust$cluster))
  points(clust$centers, col=1:length(clust$centers),
         pch=4, cex=2)
}

dat <- read.table("~/dev/hiui/16-housing.txt", header=TRUE)

cat("statData")
analyse_clust(dat$CRIM, dat$INDUS,
              as.factor(dat$CHAS))

n1 <- 1000
a1 <- c(1, 1)
r1 <- cbind(c(1, -1), c(-1, 2))
n2 <- 500
a2 <- c(-2, 2)
r2 <- cbind(c(1, -0.5), c(-0.5, 2))
dat <- rbind(mvrnorm(n1, a1, r1), mvrnorm(n2, a2, r2))

cat("randData")
analyse_clust(dat[,1], dat[,2], c(rep(1, n1), rep(2,
                                                  n2)))

