distance <- function(u, v) {
	sqrt(sum((u-v)^2));
};

kRect <- function(x) 0.5 * (abs(x)<=1)
kTriang <- function(x) (1 - abs(x))*(abs(x)<=1)
kQart <- function(x) (15/16)*(1 - x^2)^2 * (abs(x)<=1) 
kEpan <- function(x) (3/4)*(1-x^2) * (abs()<=1)
kGaus <- function(x) dnorm(x)

parsen <- function(dots, x, width, k) {
	n <- dim(dots)[2] - 1
	names <- length(names(table(dots[, 3])))
	## print(orderedDots[2,3])
	classes <- rep(0, names)
	for(i in 1:dim(dots)[1]) {
		classes[dots[i,3]] <- classes[dots[i, 3]] + k(distance(dots[i, 1:n], x)/width)
	}
	if(sum(classes) > 0) class <- names(table(dots[, 3]))[which.max(classes)]
    else class <- "none"
	return (class)
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main="Rectangular window")

for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			class <- parsen(xl, z, 0.3, kRect)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main="Triangular window")

for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			class <- parsen(xl, z, 0.3, kTriang)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main="Quartic window")

for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			class <- parsen(xl, z, 0.3, kQart)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main="Epanechnikov's window")

for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			class <- parsen(xl, z, 0.3, kEpan)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1, main="Gaussian window")

for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			class <- parsen(xl, z, 0.3, kGaus)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}