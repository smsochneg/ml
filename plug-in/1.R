library("MASS")

n <- 100

mu1 <- c(10, 15)
mu2 <- c(15, 15)

# ковариационные матрицы для котоорой разделяющая поверхность - гиперполоид
sigma1g <- matrix(c(15, 0, 0, 1), 2, 2)
sigma2g <- matrix(c(1, 0, 0, 15), 2, 2)

vyborka1g <- mvrnorm(n=n, mu = mu1, Sigma = sigma1g)
vyborka2g <- mvrnorm(n=n, mu = mu2, Sigma = sigma2g)

vyborka1g.kde <- kde2d(vyborka1g[,1], vyborka1g[,2], n = 50)
vyborka2g.kde <- kde2d(vyborka2g[,1], vyborka2g[,2], n = 50)

# ковариационные матрицы для котоорой разделяющая поверхность - элипсоид
sigma1e <- matrix(c(10, 0, 0, 1), 2, 2)
sigma2e <- matrix(c(30, 0, 0, 10), 2, 2)

vyborka1e <- mvrnorm(n=n, mu = mu1, Sigma = sigma1e)
vyborka2e <- mvrnorm(n=n, mu = mu2, Sigma = sigma2e)

vyborka1e.kde <- kde2d(vyborka1e[,1], vyborka1e[,2], n = 50)
vyborka2e.kde <- kde2d(vyborka2e[,1], vyborka2e[,2], n = 50)

# ковариационные матрицы для котоорой разделяющая поверхность - линейная
mu1 <- c(10, 15)
mu2 <- c(20, 15)

sigma1l <- matrix(c(5, 0, 0, 10), 2, 2)
sigma2l <- matrix(c(5, 0, 0, 10), 2, 2)

vyborka1l <- mvrnorm(n=n, mu = mu1, Sigma = sigma1l)
vyborka2l <- mvrnorm(n=n, mu = mu2, Sigma = sigma2l)

vyborka1l.kde <- kde2d(vyborka1l[,1], vyborka1l[,2], n = 50)
vyborka2l.kde <- kde2d(vyborka2l[,1], vyborka2l[,2], n = 50)

#########################################################################
findMu <- function(points) {
    cols = dim(points)[2]
    mu = matrix(NA, 1, cols)
    for (i in 1:cols) {
        mu[1, i] = mean(points[, i])
    }
    return(mu)
}

findSigma <- function(points, mu) {
    rows = dim(points)[1]
    cols = dim(points)[2]
    covar = matrix(0, cols, cols)
    for (i in 1:rows) {
        covar = covar + (t(points[i,] - mu) %*% (points[i,] - mu)) / (rows - 1)
    }
    return(covar)
}

getFunc <- function(sigma1, mu1, sigma2, mu2) {
  obr1 <- solve(sigma1)
  obr2 <- solve(sigma2)
  
  a <- obr1 - obr2
  b <- obr1 %*% t(mu1) - obr2 %*% t(mu2)
  
  A <- a[1,1] # x^2
  B <- a[2,2] # y^2
  C <- 2 * a[1, 2] # xy
  D <- -2 * b[1, 1] # x
  E <- -2 * b[2, 1] # y
  G <- c(mu1 %*% obr1 %*% t(mu1) - mu2 %*% obr2 %*% t(mu2)) + log(abs(det(sigma1))) - log(abs(det(sigma2)))

  return(
    function(x, y) {
      x^2 * A + y^2 * B + x*y*C + x*D + y*E + G
    }
  )
}

colors <- c("red", "green")
scalex <- 40
scaley <- 40
# plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(-scale,scale), ylim=c(-scale,scale))
# image(vyborka1g.kde)
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(0,scalex), ylim=c(0,scaley))

image(vyborka1g.kde, add=TRUE, col = hcl.colors(1000, "reds", rev = TRUE))
image(vyborka2g.kde, add=TRUE, col = hcl.colors(1000, "greens", rev = TRUE))

points(vyborka1g, pch=21, col=colors[1], bg=colors[1])
points(vyborka2g, pch=21, col=colors[2], bg=colors[2])

mu1 <- findMu(vyborka1g)
mu2 <- findMu(vyborka2g)
sigma1 <- findSigma(vyborka1g, mu1)
sigma2 <- findSigma(vyborka2g, mu2)

func <- getFunc(sigma1, mu1, sigma2, mu2)

x <- seq(0, scalex, len = 1000)
y <- seq(0, scaley, len = 1000)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5)

#####################################

# plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(-scale,scale), ylim=c(-scale,scale))
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(0,scalex), ylim=c(0,scaley))

image(vyborka1e.kde, add=TRUE, col = hcl.colors(1000, "greens", rev = TRUE))
image(vyborka2e.kde, add=TRUE, col = hcl.colors(1000, "reds", rev = TRUE))

points(vyborka1e, pch=21, col=colors[1], bg=colors[1])
points(vyborka2e, pch=21, col=colors[2], bg=colors[2])

mu1 <- findMu(vyborka1e)
mu2 <- findMu(vyborka2e)
sigma1 <- findSigma(vyborka1e, mu1)
sigma2 <- findSigma(vyborka2e, mu2)

func <- getFunc(sigma1, mu1, sigma2, mu2)

x <- seq(0, scalex, len = 1000)
y <- seq(0, scaley, len = 1000)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5)

#####################################

plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(0,scalex), ylim=c(0,scaley))

image(vyborka2l.kde, add=TRUE, col = hcl.colors(1000, "greens", rev = TRUE))
image(vyborka1l.kde, add=TRUE, col = hcl.colors(1000, "reds", rev = TRUE))

points(vyborka1l, pch=21, col=colors[1], bg=colors[1])
points(vyborka2l, pch=21, col=colors[2], bg=colors[2])

mu1 <- findMu(vyborka1l)
mu2 <- findMu(vyborka2l)
sigma1 <- findSigma(vyborka1l, mu1)
sigma2 <- findSigma(vyborka2l, mu2)

func <- getFunc(sigma1, mu1, sigma2, mu2)

x <- seq(0, scalex, len = 1000)
y <- seq(0, scaley, len = 1000)
z <- outer(x, y, func)
contour(x, y, z, levels = 0, add = TRUE, drawlabels = TRUE, lwd = 2.5)