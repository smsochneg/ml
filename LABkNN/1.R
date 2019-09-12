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
	orderedDots <- sortObjects(xl, x)
	names <- table(orderedDots[1:k, 3])
	answer <- names(sort(names, decreasing = TRUE)[1])
	return (answer)
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

for(i in 1:10){
	z <- c(runif(1, 1, 7), runif(1 , -1, 4))
	xl <- iris[, 3:5]
	k <- 10
	class <- kNN(xl, z, k)
	points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
}
