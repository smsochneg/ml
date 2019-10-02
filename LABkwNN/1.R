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

kwNN <- function(dots, x, k, q) {
	n <- dim(dots)[2] - 1
	orderedDots <- sortObjects(xl, x)
	names <- table(orderedDots[1:k, 3])
	## print(orderedDots[2,3])
	rank <- rep(0, length(names))
	for(i in 1:length(seq(k))) {
		rank[orderedDots[i,3]] <- rank[orderedDots[i, 3]] + q^i
	}
	answer <- names[which.max(rank)]
	return (names(answer))
};

LOOkwNN <- function(dots) {
	obj <- array(0, 2)
	for(q in seq(0.05, 0.1, 0.05)) {
		loo <- 0
		for(i in 1:dim(dots)[1]) {
			ee <- kwNN(dots[-i,], c(dots[i,1], dots[i,2]) , 6, q)
			loo <- loo + (ee != dots[i,3])
		}
		print(loo)
		obj[q] = loo/150
	}
	return (obj)
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)


z <- c(2.6, 0.5)
xl <- iris[, 3:5]
# LOOkwNN(iris[, 3:5]);
plot(LOOkwNN(xl), ylim=c(0,1))
k <- 6
class <- kwNN(xl, z, k, 0.5)
class <- names(class)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)

