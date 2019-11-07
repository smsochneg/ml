levellines <- function(mu, covar, title) {
  a11 <- covar[1, 1]
  a12 <- covar[1, 2]
  a21 <- covar[2, 1]
  a22 <- covar[2, 2]
  
    a11i <- a22
    a12i <- -a12
    a21i <- -a21
    a22i <- a11

  A <- a11i / det(covar)
  B <- a22i / det(covar)
  C <- a12i + a21i / det(covar)
  D <- 2*a11i*mu[1] + mu[2]*(a21i+a12i) / det(covar)
  E <- 2*a22i*mu[2] + mu[1]*(a21i+a12i) / det(covar)
  F <- a11i*mu[1]^2 + a22i*mu[2]^2 - mu[1]*mu[2]*(a21i+a12i) / det(covar)
  
  N <- function(x, y) {
    1 / (2*pi*sqrt(det(covar))) * exp(-0.5 * (x^2*A + y^2*B + x*y*C - x*D - y*E + F))
  }
  
  X <- seq(-4, 4, 0.01)
  Y <- seq(-4, 4, 0.01)
  Z <- outer(X, Y, N)

  contour(X, Y, Z, main=title)
}

par(pty="s")

levellines(c(0, 0), matrix(c(1, 0, 0, 0.3), 2, 2), "Features not corellated") 
levellines(c(0, 0), matrix(c(1, 0, 0, 1), 2, 2), "Features with same dispersion")
levellines(c(0, 0), matrix(c(1, 1, 0, 1), 2, 2), "Featires corellated")