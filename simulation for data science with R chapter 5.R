### chapter 5 Monte Carlo Methods for optimization Problems:

##Numerical optimization:
#the aim is to find the extreme values (for example, maxima or imima) of a function f(x) or of an implicit equation g(x) = 0. In focus it is therefore the optimization problem maxh(x). or in other words, we search for a value that holds:

#Basically, two kinds of approaches exist to solve a complex optimization problem, as already mentioned:
	#The pure deterministic approach
	#The stochastic approach
	
#Deterministic means in this chapter to follow strict rules to achieve the maxima without any randomness included. While the numberical deterministic solution of the problem depends on the analytical properties of the objective function h (for example, convexity and smoothness), the stochastic approach is of a more general use.

#For the following examples we use the following function, where afterwards we want to find its minimum. The optima of our modified 2D Rosenbrock function (mountains) should be at (1,1).
mountains <- function(v) {
	(1 - v[1]) ^2 + 100 * (v[2] - v[1]*v[1])^2 + 
	0.3*(0.2 - 2*v[2])^2 + 100 * (v[1] - v[2]*v[2])^2 -
	0.5*(v[1]^2 + 5*v[2]^2)
}

#The contour plot shows contour lines (also known as isolines or isopleths) of a function of two variables as a curve, where points on this line/curve have a constant value. The following graphic shown in the next couple of pages is an illustration of this same function.

#Additional note (make sure to look up what Newton-Raphson method entails). 

##Gradient Ascent and Descent:
#The gradient descent method is a first order derivative optimization method for unconstrained nonlinear function optimization. For function maximization we speak about ascent method, for minization, we call it descent. 

#the stepest descent search is an extension that goes in the direction of the line of the gradient in each step, basically an optimum step is done, also referred to as a stepest step.

#The aim is to locate the maximum/minimum of a function. For initialization, a starting point has to be selected. For this point as well as for any point reached later on, the derivative of the function is calculated. A next point is selected in direction of the gradient of the funcitons derivative at a distance of (the step size parameter) from the current point.

#A very simple example where this method is successfully is shown with this line of code.
library(animation)
grad.desc()# very interesting representation. From the looks of the graphic the path becomes more and more contorted the further to the highest slope the path reaches. This most be because of the curvature of the gradient itself thus giving off this illusion. Also the path becomes shorter and shorter the further the path reaches to the highest point.

#And a function where already this simple method fails can be seen by executing this code:
ani.options(nmax = 70)
par(mar = c(4,4,2,0))
f2 <- function(x, y) sin(1/2 * x^2 * y^2 + 3) * cos(2 * x + 1 - exp(y))
grad.desc(f2, c(-2,-2,2,2), c(-1, 0.5), gamma = 0.3, tol = 1e-04)
# Now I understand the gradients were changed from only one gradient to multiple. The following example might be a bit too deterministic to understand that it has multiple gradients to measure. Very interesting representation. Will need to experiment with this grad.desc() function a little more. 

##Newton-Raphson methods:
#The Newton-Raphson method is the most famous deterministic optimization method. At each point, the first derivative determines the line to follow to the next point, and the second derivative is used to select the direction of the line. More precisely, Newton-Raphson methods are based on the recursion:
#To look that the methods equation look at page 154.

#Wherein the matrix of the first derivative is called the gradient, and the matrix of the second derivative is known as the Hessian matrix. This method depends strongly on the starting value.

#In R a Newton-type algorithm is implemented in function nlm. The first function argument is the function to be minimized. In the following example we plug-in our function mountains and store each solution for the first 10 steps and visualize them in the following graphic. We start somewhat badly so that we can see how this algorithm falls into the local minima:
n <- 300
## to define the grid 
x <- seq(-1, 2, length.out = n)
y <- seq(-1, 2, length.out = n)
##evaluate on each grid point.
z <- mountains(expand.grid(x, y))
##contour plot:
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
##Warning in matrix(log10(z), length(x)): NaNs produced.
##starting value:
sta <- c(0.5, -1) # As the description of the Newton Raphson method. The first value (0.5) is the gradient and the second value (-1) is the Hessian matrix. I believe.

points(sta[1], sta[2], cex = 2, pch = 20)
##solutions for each of 20 steps:
sol <- matrix(, ncol = 2, nrow = 21)
for(i in 2:20){
	sol[i, ] <- nlm(mountains, sta, iterlim = i)$est
} 
##optimal solution:
sol[21,] <- nlm(mountains, sta)$est 
points(sol[21,1], sol[21,2], cex = 3, col = "red", pch = 20)
##path visually
lines(sol, pch = 3, type = "o")
##now let's start better (dashed line)

#this method did find one of the minimas but the problem was that it wasn't the lowest point. I believe that the other method will fix this problem.

##optimal solution: 
sol[21,] <- nlm(mountains, sta)$est
points(sol[21,1], sol[21,2], cex = 3, col = "red", pch = 20)
##path visually:
lines(sol, pch = 3, type = "o")
##now let's start with a better (dashed line):
sta <- c(0,-1)
for(i in 2:20){
	sol[i,] <- nlm(mountains, sta, iterlim = i)$est 
}
sol[1,] <- sta 
sol[21,] <- nlm(mountains, sta)$est
points(sta[1], sta[2], cex = 2, pch = 20)
points(sol[21,1], sol[21,2], cex = 3, col = "red", pch = 20)
lines(sol, pch = 3, type = "o")

#We can see that for this 2-dimensional example, the Newton-Raphson gradient method works well if the starting point chosen is not too bad.
#As already mentioned, the Newton-Raphson method is deterministic, that is, it always reaches the same initial starting values always at the same solution (will need to decipher this statement).

#A global optimum can be found very quickly and easily, if the function is convex/concave. However, if the function to be optimized is not concave/convex and it several local optima exist, the Newton-Raphson method will probably only find a local optimum. 

##Further general-purpose optimization methods:
#The standard function to solve general-purpose optimization problems in r is optim. Several methods are implemented:
	#The Nelder-Mead method: the default. Works well for non-differentiable functions.
	#The BFGS method: This is a quasi-Newton method, especially useful if the Hessian is unavailable or is too expensive to compute at every iteration.
	#The CG method: Used for larger problems.
	#The L-BFGS-B method: This allows box constraints for each variable. Of course, the initial value must satisfy these constraints.
	#The SANN method: It's implemented using a simulated annealing approach. It uses a Metropolis function for the acceptance probability. It is not a general purpose method and it is relatively slow, but it can be very useful in receiving a good solution on a very rough surface.
		#(this method sounds like an illustration of the stochastic approach perhaps).
		
#We will apply these methods to our function mountains. Note that we need a for loop to save the solution after every step to see how the algorithms work on this problem. From the resulting graphic we see that it highly depends on the starting value if a local or global maxima is reached. Note that for all methods, the global optima at (1,1) is approximately reached, not exactly, but it is a matter of computation time to come closer and closer to (1,1).

##wrapper for all methods of optim:
optims <- function(x, meth = "Nelder-Mead", start = c(0.5, -1)){
	sol <- matrix(, ncol = 2, nrow = 21)
	sol[1,] <- start
	for(i in 2:20){
		sol[i,] <- optim(start, mountains, method = meth, control = list(maxit = i))$par
	}
sol[21,] <- optim(start, mountains)$par
points(start[1], start[2], pch = 20, cex = 2)
points(sol[21,], sol[21,], pch = 20, col = "red", cex = 3)
lines(sol[,1], sol[,2], type = "o", pch = 3)
}

##plot lines for all methods:
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
optims() #Nelder-Mead the default
optims("BFGS")
optims("CG")
optims("L-BFGS-B")
optims("SANN")
optims("Brent")
optims(start = c(1.5, 0.5))
#This representation illustrates that all of these deterministic methods rely heavily on the starting point (They will always seek out the closest maxima or minima global or local).

#It should be noted that other functions and packages are also useful, such as the R packages nloptr and optimx. Lastly, it should be mentioned that the R package ROI provides an interface/wrapper to many solvers. It consists of an optimization infrastructure, a sophisticated framework for handling optimization problems in R.

##Dealing with stochastic optimization:
#By using stochastic optimization one can find a different solution with the same starting values. This should also allow us to not be stuck with the local maximas or minimas.

##Simplified procedures (Star trek, Spaceballs, and spaceballs princess).
#To move from this deterministic approach to an approach which includes randomness, one can just sample points over the whole distribution of f(). If the probability of selecting a point is equal over the whole space, the probability to find a solution, say A, is equal to the deterministic approach.

#This approach can lead to good results for low-dimensional problems, but it is computationally too slow to find good solutions for higher dimensions. Naturally, this approach can be modified by a sequential draw of points, for example, starting by drawing a set of points and drawing the next set of points depending on the values of the first set of points. For the maximization in a two-dimensional problem this means that more data points are selected (for a follow-up draw of points) around higher points. We will consider this, but only choose a set of points and mark the extreme value. If f is compact, we may draw from a uniform distribution m observations. 

#The maximum in red found with an evaluation of m = 1500 draws between -2 and 5 from a bivariate uniform distribution:

##define grid:
n <- 1500
set.seed(1234567)
x1 <- runif(n, min = -2, max = 5)
y1 <- runif(n, min = -2, max = 5)
z1 <- matrix(, ncol = n, nrow = n)
##evaluate on each grid point:
for(i in 1:n){
	for(j in 1:n){
		z1[i, j] <- mountains(c(x1[i], y1[j]))
	}
}

##determine the optima:
w <- which(z1 == min(z1), arr.ind = TRUE)
##plot results:
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)

points(x1[w[1]], y1[w[2]], pch = 20, col = "red", cex = 3)
points(x1, y1, pch = 3)

#This approach is, of course, too simplistic, but remember we already mentioned an improvement of Spaceballs. Let's consider the Spaceballs princess approach, that is, let's iterate to get better solutions. The idea is the following:
	#Sample points randomly, compute the values of the function, and select those points with highest values.
	#Repeat, at the fittest points located, draw values from a normal distribution, and again select the fittest points. 
	
#This implemented in the package RCEIM, which can be used for multidimensional function optimization. The method does not impose strong conditions on the function to be optimized, and we can easily apply it to out two mountains problem. Again, we save all immediate solutions and display all fittest points for each step of the algorithm. The plot in the following graphic shows those points from light gray (early stage) to black (fittest points at step 20). We also plot red crosses for the best solution in each step. All are located nearby the optimum. The final solution is marked by a large red filled circle:
library(RCEIM)
set.seed(123)
sol <- best <- list()
##Save the solution for each set.
for( i in 2:20){
	a <- ceimOpt(mountains, nParam = 2, maxIter = i)
	sol[[i]] <- a$EliteMembers
	best[[i]] <- a$BestMember
}

##plot the results for each step 
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
greys <- grey(rev(2:20/20 - 0.099))
for( i in 2:20){
	points(sol[[i]][,1], sol[[i]][,2], col = greys[i])
	points(best[[i]][1], best[[i]][2], col = "red", pch = 3)
}
points(best[[i]][1], best[[i]][2], col = "red", pch = 20, cex = 3)

##Metropolis-Hastings revisited:
#The a very good explaination of this method consult page 163 of the book. This passage is a bit too escoteric for me to understand. Will need to gain more knowledge of this concept before I can understand what to look into in more detail.

#the following code implements this kind of random walk Metropolis Hastings algorithm for an optimization problem in two dimensions:
##Simple random walk Metropolis Hastings:
rmh <- function(n = 20, start = c(0, -0.5), stepmult = 10){
	x <- matrix(, ncol = 2, nrow = n)
	x[1,] <- start 
	sol <- mountains(start)
	for(i in 2:n){
		x[i,] <- x[i-1,] + rmvnorm(1, mean = c(0,0), sigma = stepmult * diag(2) / n)
		solnew <- mountains(x[i,])
		#accept only a better solution:
		if(solnew > sol) x[i,] <- x[i-1,]
		if(solnew < sol) sol <- solnew 
	}
	return(x)
}

#Let's take two walks from the same starting point:
library(mvtnorm)
set.seed(12345)
n <- 200
x1 <- rmh(n, start = c(1.5, 0))
x2 <- rmh(n, start = c(1.5, 0))

#Again the solutions are visualized for each step of the algorithm. In the following graphic it can be seen that once the algorithm is trapped into the local minima while at the same starting point, the other time the algorithm was successful to find the optimal solutions.

par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)

points(x1[1,1], x1[1,2], pch = 4, cex = 3)
points(x2[n, 1], x2[n,2], pch = 20, col = "red", cex = 3)
points(x1[n,1], x1[n, 2], pch = 20, col = "red", cex = 3)
lines(x1[,1], x1[,2], type = "o", pch = 3)
lines(x2[,1], x2[,2], type = "o", col = "blue", lty = 2)

##gradient based stochastic optimization:
#check page 165 for a very good description of this method. Currently I'm unable to understand this section of the book. Will need to look into other resources on this matter in the future.

#The following code implements the stochastic gradient approach described in this page. Starting values as well as parameters for the sequences of alpha j and beta i are included as function parameters. 
stoGrad <- function(start = c(0, -0.5), j = 1500, p = 0.1){
	theta <- matrix(start, ncol = 2)
	diff <- iter <- 1
	alpha <- sapply(1:100, function(x) 1/(j+1))
	beta <- sapply(1:100, function(x) 1/(j+1)^(p))
	while(diff > 10^-5 & !is.nan(diff) & !is.na(diff)){
		zeta <- rnorm(2)
		zeta <- zeta / sqrt(t(zeta) %*% zeta)
		grad <- alpha[iter] * zeta * (mountains(theta[iter,] + beta[iter] * zeta) - mountains(theta[iter,] - beta[iter] * zeta)) / beta[iter]
		theta <- rbind(theta, theta[iter,] - grad)
		diff <- sqrt(t(grad) %*% grad)
		iter <- iter +1
	}
	list(theta = theta[1: (iter-1),], diff = diff, iter = iter - 1)
}

#The following plot shows the solution to the stochastic gradient method. Even if it is a simple 2-dimensional example, it took some time to find a good choice for the sequence of beta_i and alpha_i to achieve convergence and to find a good solution:
set.seed(123)
s1 <- stoGrad()
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
plotLine <- function(x, ...){
	lines(x$theta[,1], x$theta[,2], type = "o", ...)
	points(x$theta[x$iter, 1], x$theta[x$iter, 1], pch = 20, col = "red", cex = 3)
}

plotLine(s1, pch = 3)
points(0, -0.5, pch = 20, cex = 1.5)
plotLine(stoGrad(), col = "blue", pch = 4)
plotLine(stoGrad(start = c(1.5, 0)), pch = 3, lty = 2)
plotLine(stoGrad(start = c(1.5, 0)), pch = 4, lty = 2, col = "blue")
points(1.5, 0, pch = 20, cex = 1.5)

#The implemented stochastic gradient approach is sensible to parameters j and p. We can see that modifying the parameter p to higher values leads to better results:
set.seed(123)
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
plotLine <- function(x, ...){
	lines(x$theta[,1], x$theta[,2], type = "o", ...)
	points(x$theta[x$iter, 1], x$theta[x$iter, 1], pch = 20, col = "red", cex = 3)
}
plotLine(s1, pch =3)
points(0, -0.5, pch = 20, cex = 1.5)
plotLine(stoGrad(p = 2.5), col = "blue", pch = 4)
plotLine(stoGrad(start = c(1.5,0), j = 1500, p = 2.5), pch = 3, lty = 2)
plotLine(stoGrad(start = c(1.5, 0), j = 1500, p=2.5), col = "blue", pch = 4, lty = 2)
points(1.5, 0, pch = 20, cex = 1.5)# Interesting my solution seems to be different from what the author obtained from using this same method. Most likely I copied the code wrong will need to go back to this problem later on in my studies. 

#the package nloptr has an improved stochastic ranking evolution strategy for nonlinearly constrained global optimization implementated.
library(nloptr)
set.seed(123)
##mountains function with modified function parameters:
mountains1 <- function(x) ((1-x[1]) ^2 + 100 * (x[2] - x[1] * x[1])^2 + 0.3*(0.2 - 2*x[2])^2 + 100 * (x[1] - x[2]*x[2])^2 - 0.5*(x[1]^2 + 5*x[2]^2))

x0 <- c(0.5, -1)
lb <- c(-3, -3)
ub <- c(3, 3)
sol <- matrix(, ncol = 2, nrow = 21)
##solution on each step:
for(i in 1:20){
	sol[i,] <- isres(x0 = x0, fn = mountains1, lower = lb, upper = ub, maxeval = i)$par 
}
par(mar = c(4,4,0.5,0.5))
contour(x, y, matrix(log10(z), length(x)), xlab = "x", ylab = "y", nlevels = 20)
##start 
points(sol[1,1], sol[1,2], pch = 20, cex = 2)
##optima found:
sol[21,] <- isres(x0 = x0, fn = mountains1, lower = lb, upper = ub)$par
points(sol[21,1], sol[21,2], pch = 20, col = "red", cex = 3)
##way to optima 
lines(sol[,1], sol[,2], type = "o", pch = 3)