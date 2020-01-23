library(MASS)  
library(kernlab)

Normalization <- function(xl) {     
	n <- dim(xl)[2] - 1     
    for(i in 1:n)      {         
    	xl[,i] <- (xl[,i]-mean(xl[,i]))/sd(xl[,i])      
    }     
    return(xl) 
}
  
Prepare <- function(xl) {     
    l <- dim(xl)[1]     
    n <- dim(xl)[2]-1     
    xl <- cbind(xl[,1:n], seq(from = -1, to = -1, length.out = l), xl[,n+1]) 
} 
   
quadLoss <- function(x) {     
    return ((x-1)^2) 
}
  
hebbLoss <- function(x) { 
    return (max(-x, 0)) 
}
  
SGradient <- function(xl, eta = 1, lambda = 1/5, id) {
    l <- dim(xl)[1]     
    n <- dim(xl)[2] - 1        
    w <- c(1/2, 1/2, 1/2)     
    iterCount <- 0        
    Q <- 0     
    for (i in 1:l){       
      wx <- sum(w*xl[i,1:n])            
      margin <- wx*xl[i,n+1] 
      if (id == 1) Q <- Q + quadLoss(margin)  
      else if (id == 2) Q <- Q + hebbLoss(margin)
    }        
	count <- 0
    repeat {
		count <- count + 1
      	margins <- array(dim=l)                  
      	for (i in 1:l) {             
    		xi <- xl[i,1:n]             
        	yi <- xl[i,n+1]                          
        	margins[i] <- crossprod(w,xi)*yi          
      	}              
		errorIndexes <- which(margins <= 0)              
		if (length(errorIndexes) > 0) {             
			i <- sample(errorIndexes, 1)             
			iterCount <- iterCount+1                           
			xi <- xl[i,1:n]             
			yi <- xl[i,n+1]     
			wx <- sum(w*xi)                         
			margin <- wx*yi      
			if (id == 1) {
				ex <-   quadLoss(margin)     
				eta <- 1/sqrt(sum(xi*xi))  
				w <- w-eta*(wx-yi)*xi  
			}
			else if (id == 2) {
				ex <- hebbLoss(margin)
				eta <- 1/iterCount   
				w <- w+eta*yi*xi 
			}

			if (count %% 15 == 0) abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 1, col = "blue")

			Qprev <- Q    
			Q <- (1-lambda)*Q+lambda*ex  
			if (abs(Qprev-Q)/abs(max(Qprev,Q)) < 1e-5) break 
			if (iterCount > 1000) break
		}         
      	else break         
    }
    return (w)
}

sigma1 <- matrix(c(10, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(10, 0, 0, 1), 2, 2)
mu1 <- c(10, 15)
mu2 <- c(25, 14)
count <- 100

xy1 <- mvrnorm(n=count, mu1, sigma1) 
xy2 <- mvrnorm(n=count, mu2, sigma2)  
xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
colors <- c("green", "white", "red")  

xlNorm <- Normalization(xl) 
xlNorm <- Prepare(xlNorm) 

## ADALINE 
plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "ADALINE") 
w <- SGradient(xlNorm, id = 1)  
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "black")  
points(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1)


##Hebb
plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Hebb's rule") 
w <- SGradient(xlNorm, id = 2)     
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "black") 
points(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1)
