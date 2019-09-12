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

kNN <- function(dots, x) {
	orderedDots <- sortObjects(xl, x);

	answer <- orderedDots[1, ]
	return (answer[1, 3])
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

z <- c(1, 3)
xl <- iris[, 3:5]
class <- kNN(xl, z)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
