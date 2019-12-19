library("MASS")

n <- 100

mu1 <- c(10, 15)
mu2 <- c(15, 15)

sigma1g <- matrix(c(15, 0, 0, 1), 2, 2)
sigma2g <- matrix(c(1, 0, 0, 15), 2, 2)

vyborka1g <- mvrnorm(n=n, mu = mu1, Sigma = sigma1g)
vyborka2g <- mvrnorm(n=n, mu = mu2, Sigma = sigma2g)

vyborka1g.kde <- kde2d(vyborka1g[,1], vyborka1g[,2], n = 50)
vyborka2g.kde <- kde2d(vyborka2g[,1], vyborka2g[,2], n = 50)

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


Pyj <- function(x, mu, sigma){
    return( (1/(sigma*sqrt(2*pi))) * exp(-1 * ((x - mu)^2)/(2*sigma^2)) )
}

classificator <- function(x, mu, sigma) {
  res <- 0
  l <- length(x)
  
  for (i in seq(l)) {
    p <- Pyj(x[i], mu[i], sigma[i, i])
    res <- res + log(p)
  }
  
  return(res)
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

x <- seq(0, 40, 40/80)
y <- seq(0, 40, 40/80)

for (i in x) {
  for (j in y) {
    res1 <- classificator(c(i, j), mu1, sigma1)
    res2 <- classificator(c(i, j), mu2, sigma2)
    color <- ifelse(res1 > res2, "red", "green")
    
    points(i, j, pch = 21, col = color)
  }
}
