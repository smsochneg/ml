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

kwNN <- function(dots, x, k, q=0.8) {
	n <- dim(dots)[2] - 1
	orderedDots <- sortObjects(dots, x)
	names <- table(orderedDots[1:k, 3])
	## print(orderedDots[2,3])
	rank <- rep(0, length(names))
	for(i in 1:k) {
		rank[orderedDots[i,3]] <- rank[orderedDots[i, 3]] + q^i
	}
	answer <- names[which.max(rank)]
	return (names(answer))
};

LOOkwNN <- function(dots) {
	loo <- matrix(rep(0, length(seq(0.05, 0.95, 0.05))))

	for(q in seq(0.05, 0.95, 0.05)) {
		for(i in 1:dim(dots)[1]) {
			ee <- kwNN(dots[-i,], c(dots[i,1], dots[i,2]) , 6, q)
			loo[q*20] <- loo[q*20] + (ee != dots[i,3])
		}
		loo[q*20] = loo[q*20] / 150
		# print(loo)
		# obj[q] = loo/150
	}
	# rownames(loo) <- seq(0.05, 0.95, 0.05)

	return (loo)
};

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

z <- c(2.6, 0.5)
xl <- iris[, 3:5]
# LOOkwNN(iris[, 3:5]);
loo <- LOOkwNN(xl)
plot(loo, ylim=c(0,0.1), type="l")

ptext <- paste("q=", 0.05*which(loo == min(loo))[1], "\nLOO=", min(loo))
text(5, 0.05, labels=ptext)

# plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
# for(i in seq(1,7,0.1)) {
# 	for(j in seq(0,2.5,0.1)) {
# 			z <- c(i, j)
# 			xl <- iris[, 3:5]
# 			k <- 6
# 			class <- kwNN(xl, z, k)
# 			points(z[1], z[2], pch = 22, col = colors[class], asp = 1)
# 	}
# }

proof <- function() {
	vyborka <- data.frame(
		x=c(2,2,1.5,0.5,1),
		y=c(3,2.5,2.5,0.5,0.5),
		Species=c("setosa","setosa","setosa","versicolor","versicolor")
	)

	show <- c(1, 1)

	plot(vyborka[,1:2], pch = 21, bg = colors[vyborka$Species], col = colors[vyborka$Species], asp = 1)
	class <- kwNN(vyborka, show, 5)
	points(show[1], show[2], pch = 22, col = colors[class], asp = 1)
}

proof()
