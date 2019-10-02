distance <- function(u, v) {
	sqrt(sum((u-v)^2));
};

sortObjects <- function(dots, x) {
	l <- dim(dots)[1]
	n <- dim(dots)[2] - 1
	## Создаём матрицу расстояний
	distances <- matrix(NA, l, 2)
	for (i in 1:l)
	{
		distances[i, ] <- c(i, distance(dots[i, 1:n], x))
	}

	ordered = dots[order(distances[, 2]), ];

	return (ordered);
};

kNN <- function(dots, x, k) {
	orderedDots <- sortObjects(dots, x)
	answer = array(NA, length(k))
	for(i in seq(length(k))){
		names <- table(orderedDots[1:k[i], 3])
		answer[i] <- names(sort(names, decreasing = TRUE)[1])
	}
	return (answer)
};

LOO <- function(dots, k) {
	loo <- array(0, k)
	for(i in 1:dim(dots)[1]) {
		ee <- kNN(dots[-i,], c(dots[i,1], dots[i,2]) , seq(k))
		loo <- loo + (ee != dots[i,3])
	}
	loo <- loo / dim(dots)[1]
	return (loo)
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

loo <- LOO(iris[, 3:5], 120)
plot(loo, type="l")

points(which(loo == min(loo)), min(loo), pch = 21, asp = 1)
ptext <- paste("k=", which(loo == min(loo)), "\nLOO=", min(loo))
text(which(loo == min(loo))+17, min(loo)+0.1, labels=ptext)


plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)


for(i in 1:10) {
	z <- c(runif(1, 1, 7), runif(1 , -1, 4))
	xl <- iris[, 3:5]
	k <- 6
	class <- kNN(xl, z, k)
	points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
}

plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
for(i in seq(1,7,0.1)) {
	for(j in seq(0,2.5,0.1)) {
			z <- c(i, j)
			xl <- iris[, 3:5]
			k <- 6
			class <- kNN(xl, z, k)
			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
	}
}
