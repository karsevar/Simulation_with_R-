### Chapter 4 Simulation of Random Numbers:
# Generally speaking, the statistics and probability theory is based on abstract concepts of probability space and random numbers. An elegant mathematical theory has been developed, which takes its starting point from random numbers.
#Specially applied research areas such as computational statistics, data science, and statistical simulation employ the concept of random numbers. Hereby, often a large number of independent and identically distributed (i.i.d) random numbers are generated/needed, especially for simulation purposes.

#However, in computer applications, surprisingly the random numbers are mostly simulated with deterministic algorithms, which is somehow in contradiction with the basic theory. 

##Real random numbers:
#Generating real (true) random numbers should be realizations of independent identically distributed random variables, and they should be unpredictable, meaning that the next generated number is unpredictable from the previously generated random numbers. The main question is (outside of lottery tickets and casinos) are random numbers important for the fields of statistics?

#True random number generators have some disabvantages in applications in data science and statistical simulation:
	#They are often computationally expensive to generate.
	#The implementation is usually difficult and time-consuming, and thus more costly.
	#Reproducibility is not a given.
	#Whether random numbers will be generated is normally not checked.
	
#The R package , random, directly accesses www.random.org. With the facilities of this website, true random numbers can be collected. The random number generator is based on atmospheric noise and the measurement of gamma-rays that hit the surface of the Earth. Using the randomNumbers function, random numbers are streamed from this website.
library(random)
x <- randomNumbers(n = 10000, col = 2, min = 0, max = 1e+06, check = TRUE) / 1000000
# That's funny this operation required Xquartz to be ran. 

#As a first indicator of the random number generator working properly, the original values are plotted against the lagged values. This means that the first value is plotted against the second, the second value against the third, and so on. If there is a certain structure visible in the following figure, the values might be auto-correlated.
n <- length(x)
df <- data.frame(x1 = x[1:(n-1)], x2 = x[2:n])
library(ggplot2)
ggplot(df, aes(x = x1, y = x2)) + geom_point(size = 0.1) + xlab("random numbers from random.org") + ylab("lag 1")

##simulating pseudo random numbers:
#We can distinguish between (at least) two kinds of pseudo random number generators:
	#Arithmetic random number generators: they are based purely on, as the name suggests arithmetic. 
	#Recursive arithmetic random number generators: They are based on the calculation of a new random number from one of more previous numbers. Reproducibility is given by setting an initial value (seed) to ensure that exactly at that point of rerunning the program, the same random numbers are generated.
	
##Congruential generators:

##Linear and multiplicative congruential generators:
#Randu, which was constucted by IBM and used for many years, acting as a standard generator for random number at IBM mainframes. 
#To best observe the differences and the bad behaviour of Randu, the following code produces interactive 3D rotatable graphics. By running the following code it can be clearly seen that the points fall in 15 two-dimensional planes:
library(rgl)
seed <- 123
result<- c()# interesting it seems that the author for got to identify the result object as a vector data structure. Really funny that he seems to forget about this subtle problem.
randu <- function(n){
	for(i in 1:n) {
		seed<<- (65539 * seed) %% (2^31)
			result[i] <- seed / 2^31
	}
	return(result)
}

plot3s <- function(func, n = 10000) {
	x <- func(n)
	require("rgl")
	x1 <- x[1:(n-2)]
	x2 <- x[2:(n-1)]
	x3 <- x[3:n]
	plot3d(x1, x2, x3, size = 3)
	play3d(spin3d())#Good to know. The spin3d() function spins the grahic for you without have to manipulate the graphic yourself.
}

#To see the results, run the following lines:
plot3s(randu)
plot3s(runif)# (Mersenne-Twister) R's standard generator. this generator seems more random since the randu generator seemed to have generated 15 panes of random samples.

#Since Randu was widely used in the early 1970s, many results from that time are seen as suspicious. The reason for using these parameter values for this linear congruential generator is that the random numbers could be drawn quickly using the special features of some computer hardware.

##Lagged Fibonacci generators:
#Another kind of generators is defined by the Fibonacci type of generators. This class of random number generator is aimed at being an imporvement on the linear congruential generator discussed earlier.

##More generators:
#The default random number of R is the Mersenne Twister algorithm. This generator ensures uniform distribution in 624 dimensions. 
#This can be seen through the command:
RNGkind()

#One realizes that the Mersenne Twister algorithm is the default in R. In addition, the inversion method uses the inversion method to transfer values from uniform to a normal distribution. 
ok <- RNGkind()
op <- par(mfrow = c(1,2), mar = c(3,4,2,2))
set.seed(111)
hist(rnorm(1000), mean = "Mersenne Twister, Inversion", freq = FALSE, xlim = c(-4,4), ylim = c(0,0.43), cex.main = 0.7)
curve(dnorm(x), col = 2, lty = 1, lwd = 2, add = TRUE)
RNGkind("Super", "Box-Muller")
RNGkind()
hist(rnorm(1000), main = "Super-Duper, Box-Muller", freq = FALSE, xlim = c(-4,4), ylim = c(0,0.43), cex.main = 0.7)
curve(dnorm(x), col = 2, lty = 1, lwd = 2, add = TRUE)

#However, many more generators can be used even in base level R.
#When generating random numbers, we can immediately also think of random walks as an example. For this example, 200,000 random numbers are generated and stored in a matrix of a 100,000 observations and 2 columns.

#On a random walk, it is assumed that a drunken man who is too drunk to even exhibit a sense of direction will come back to the pub that he left, and also that he can find the way to his apartment whenever can he still manage to walk. We also assume that he is in good physical shape so that he can walk very, very long (an infinite distance). 

#I really need to find out how the author created this graphic representation and the other applications of the drunken random walk hypothesis. 

##Simulation of non-uniform distributed random variables:
#So far, the simulation of the same random variable was discussed. In fact, the generation of uniform random numbers is a very important step. The method for generating non-uniform random numbers are different. The main aim is to transform randome numbers from a uniform distribution to another distribution. Generally, a uniformly distributed random variable, can accordingly be transformed and modified to obtain other distributions.

##The inversion method:
#The condition is that uniformly distributed random numbers are already generated in the interval [0,1]. The inversion method takes advantage of the fact that a distribution function is also defined in the interval [0,1].

#A prerequisite for the application of an inversion process is therefore the existence of the analytic form of the inverse function, or at least of an approximation of the inverse function. If approximated, the quality of random numbers depends heavily on how well the inverse function can be approximated. 

#Let's get into practice and start with a very simple discrete distribution: the Bernoulli distribution. From random numbers following a uniform distribution, it is very simple to transfer them to a Bernoulli distribution that is determined by one parameter (pi). Using R, this could look like (pi = 0.2):
??runif()
u <- runif(100, 0, 1)
ifelse(u <= 0.2, 0, 1)

#How can we show this graphically? In the following figure, we see that a random number equal to 0.554 was drawn from U(0,1). Since it is larger than 0.2, it is projected to be 1.

#Let's switch to R. With the inversion method and the function runif(), we can simulate exponentially distributed random numbers using the inverse exponential function. The resulting figure shows the simulated exponential distributions with a different parameter, lambda.
library(MASS)
invExp <- function(n, lambda = 1) {
	u <- runif(n)
	x <- -(1/lambda) * log(u)
	return(x)
}

lambda <- c(0.5, 1, 2, 5)
par(mar = c(3,3,3,0), mfrow = c(2,2))
for(l in lambda) {
	sample <- invExp(10000, 1)
	truehist(sample, nbins = 20, col = "limegreen", main = paste("lambda =", l), xlab = "")
	curve(dexp(x, 1), from = 1e-10, add = TRUE)
}

#We also see that the inversion method is very easy to apply as soon as the inverse distribution function is known. However, F^-1(u) is often analytically difficult or impossible to compute. Even the inverse of the normal distribution is not known exactly and has to be approximated. Approximation is often done by fitting polynomials. 

##The alias method:
#This method is used to simulate categorical variables.

#The function sample() is used to simulate discrete random variables in R.

sample(1:3, 10, replace = TRUE, prob = c(3/7, 1/7, 3/7))

#Basically, the alias method can be used to simulate discrete random numbers with a given probability function.

#The basic rule is that the random numbers are drawn from a rectangle, where the height of the rectangle is 1/n, and the width is given by n, with n being the number of different categories.

#To generate the random numbers, the following algorithm will be used:
	#Simulate two random numbers u_1 and u_2 independently drawn from 
#U ~ U(0,1)
	#Set z = |nu_1|
	#If u_2 <= r_z return X_z, otherwise return a_z.
	
##Estimation of counts in tables with log-linear models:
#Simulating the values of a table is needed for constructing advanced independence tests (if the cell values of a table are independent to each other) or if only margins are available but the (expected) cell values are needed.

#the cell values can also be estimated using log-linear models. This is not only useful in the case of independence tests, but also for other applications to estimate frequencies, for example, in the research area of statistical disclosure control, whereas small estimated frequencies violate laws on privacy. 

#Using log-linear models to estimate the cell values (frequencies), it is important to assume a certain distribution assumption of counts. Generally, the assumption that the underlying distribution of cell counts is a Poisson distribution is well accepted. 

#The focus is on the cell probabilities. These cell probabilities can be modeled with alogit link function in a regression model:
#The inverse of the logit function is called logistic function. If logit (pi) = z, then pi = e^z / 1+e^z.

#Example of this using precipitation values:
x <- data.frame("spring" = c(275, 56, 52, 65, 37, 23), "summer" = c(302, 20, 29, 17, 15, 5), "autumn" = c(375, 43, 53, 52, 54, 50), "winter" = c(198, 37, 44, 69, 58, 42))

#the following cell values are estimatedwith a log-linear model:
xx <- expand.grid(rownames(x), colnames(x))# all combinations
x1 <- xx[,1]
x2 <- xx[,2]
y <- as.vector(t(prop.table(x))) #cell probabilities. So in other words, the prop.table() is actually a function in base R that expresses table entries as fraction of marginal table (according to the documentation).
form <- y ~ x1:x2 #model1
mod <- glm(form, family = "poisson") # The glm() function is used because the model is mapping the probability of a categorical value. There for the distribution is not assumed as normal.
pred <- (predict(mod)) # prediction 
pred <- exp(pred)/ (1+exp(pred)) #transf. with logistic function.
round(matrix(pred, ncol = 4, byrow = TRUE) * sum(x)) #table 
#these expected values can now be inputted to an (pi) = z test of independence.

##Rejection sampling:
#How to simulate random numbers from a normal distribution when we don't know the inverse of the normal distribution? How to simulate random numbers from a Beta distribution? How to generally simulate random numbers from a distribution wher we know the density function?
#the answer is to use rejection sampling: a very intuitive and simple (but very powerful) method to simulate random numbers from almost any distribution.

#the rejection method is based on densities rather than distribution functions as used with the inversion method. The basis is drawing numbers from an easy to simulate distribution and accepting them based on a certain acceptance criteria (or rejecting them based on a rejection criteria).

##Simulating values from a normal distribution:
#As a first example, we simulate random numbers from a normal distribution using rejection sampling.
#As proposal distribution, the Cauchy distribution is taken. For the Cauchy distribution, the inverse of the distribution function can b analytically expressed without any approximations. 

#the following blanks in this chapter's notes can be seen on page 109. Within this part of the book the author talks about the density of the standard normal distribution equation and the corresponding Cauchy distribution. Again I really need to learn more about mathematics before I can even comprehend this part of the chapter. 

#For illustration, the theoretical values of a Cauchy and a normal distribution are calculated:
x <- seq(-5, 5, length = 200)
dc <- dcauchy(x, location = 0, scale = 1)#The Cauchy distribution can be created through the function dcauchy in base R.
dn <- dnorm(x, mean = 0, sd = 1) # Again the dnorm() function can be seen in the Book of R (which is perfectly described by Tilman Davies).
 
#The comparison of the two densities by plotting their estimated densities is done with the following code snippet. The output is presented in the following graphic:
par(mfrow = c(1,2))
plot(x,dn, type = "l")
lines(x, dc, lty = 2, col = "blue")
legend("topright", col = c("black","blue"), lty = c(1,2), legend = c("normal", "Cauchy"), cex = 0.5)
plot(x, dn/dc, type = "l")

#The optimal choice for parameter a is determined next. The optimal choice is the minimal value where a * h(x) fully covers f(x) for any x:
foo <- function(x) dnorm(x)/dcauchy(x)
opt <- optimize(foo, c(0, 4), maximum = TRUE)
a <- opt$objective
a
ah <- a * dc
ah# This is for the Cauchy distribution.

#The densities of the Cauchy distribution as well as the normal distribution are both below a*h(x) for any x and h(x):
plot(x, dn, type = "l", ylim = c(0,0.5), lwd = 2)
lines(x, dc, col = "blue", lty = 2)
lines(x, ah, col = "blue", lty = 2, lwd = 2)
legend("topright", col = c("black","blue","blue"), lty = c(1,2,2), lwd = c(1,1,2), legend = c("normal","Cauchy", "a * Cauchy"), cex = 0.5)
#I take back my initial description of ah. this object is actually the distance between the normal distribution and the Cauchy distribution.

#The difference between a*h(x) is illustrated as follows:
plot(x, dn, type = "l", ylim = c(0,0.5), lwd = 2)
polygon(x, dn, col = "gray")
polygon(c(x, rev(x)), c(dn, rev(ah)), col ="blue")# weird so the taller distribution is actually a * h(x). I really need to learn some more math. this is saddly too advanced for me.

#The acceptance probability can be written as a function, as well as the rejection sampling.
alpha <- function(x) {
	dnorm(x) / (1.520347 * dcauchy(x))
}# The 1.520347 value is actually from the opt$optimize object from the last step. The value is again the parameter a which is the minimal value where a*h(x) fully covers f(x) for any x.
rejectionNorm <- function(n) {
	x <- rcauchy(10000, 0, 1)
	u <- runif(10000)
	return(na.omit(ifelse(u <= alpha(x), x, NA)))
}

#We can now simulate random numbers from a normal distribution using rejection sampling, and se show the corresponding empirical distribution by a histogram, and we also show the theoretical density curve.
set.seed(123)
x <- rejectionNorm(10000)
hist(x, prob = TRUE)
curve(dnorm(x), lty = 1, lwd = 2, add = TRUE)

##Simulating random numbers from a Beta distribution:
#We already showed how to use the rejection method for simulating normally distributed values. But, how does this work for other distributions? The answer is easy: the same as before. We use a proposal distribution where two issues should be fulfilled:
	#The acceptance probability should be high. Note that this issue is not as important as it was earlier since considerable good results can often be produced with a low acceptance rate, and simulating random numbers from a proposal distribution is often note time consuming.
	#The proposal density times s should cover the whole range of f(x).
	
#Let Z ~ Beta(2,2) be a random variable with density f(z) = 6z(1 - z) in [0,1]v. 
#As proposal density, we can (always) use a U(0,1) distribution, as the following figure shows:
curve(dbeta(x, shape1 = 2, shape2 = 2), from = 0, to = 1, xlab = "", ylab = "", main = "") #a * h(x)

abline(h = 1.5, lty = 2)

# Won't get into too much detail about how the author created the acceptance probability equation but according to him it is 4z(1 - z):
rsBeta <- function(n) {
	z <- runif(n)
	u <- runif(n)
	ind <- (u <= 4 * z * (1 - z))
	return(z[ind])
}
set.seed(123)
sample1 <- rsBeta(10000)
acceptS <- length(sample1) / 10000
acceptS

#Let's plot this, we define a function for it:
library(MASS)
plot1 <- function(s, shape1 = 2, shape2 = 2){
	truehist(s, h = 0.1, xlim = c(0,1), ylim = c(0,2), col = "white", main = "", xlab = "")
	curve(dbeta(x, shape1 = shape1, shape2 = shape2), from = 0, to = 1, add = TRUE)
	d <- density(s, from = 0, to = 1, adjust = 2, kernel = "gaussian")
	lines(d$x , d$y, lty = 2)
	legend("topright", legend = c("true density", "density of simulated values"), col = c(1,1), lty = c(1,2), cex = 0.6)
}

#the following graphic shows our results from simulating vaues of a Beta(2,2) using the rejection sampling approach.

plot1(sample1)

#In the previous example, we also see that the acceptance rate is not as bad; we even took the easiest proposal distribution.

#We can extend the previous example by simulating random numbers from a distribution in an arbitrary interval. For this purpose, it is necessary to calculate the upper bound a. For alpha = 2.5 and beta = 6.5, the following code does the job:

rsBeta2 <- function(n, shape1 = 2.5, shape2 = 6.5){
	a <- optimize(f = function(x) {dbeta(x, shape1, shape2)},
	interval = c(0,1), maximum =TRUE)$objective
	z <- runif(n)
	u <- runif(n, max = a)
	ind <- (u <= dbeta(z, shape1, shape2))
	return(z[ind])
}
sample2 <- rsBeta(10000)
sample2
acceptS2 <- length(sample2)/10000
acceptS2

plot1(s = sample2, shape1 = 2.5, shape2 = 6.5)# Not really sure if this is what the author had in mind I believe that I will have to rewrite the plot1() function in a number of junctions. Namely the kernel argument from "guassian" into another distribution. The problem with this representation is that the true distribution is displayed as a normal distribution. 

##Truncated distributions:
#To generate random numbers within an interval [a,b], all numbers outside this interval are discarded using a rejection method. But this can be very inefficient when the interval is small in comparison to the whole range of values that must be accepted or regjected. So, how to avoid rection values but simulate only values in such an interval?

#Faster than refection here is the inversion method. 

#As an example, we look at the Cauchy distribution. We only wanted values in an interval [4,5]. With the rejection method, we would reject this percentage of values, and to simulate 10 values in this interval, we could do this:
#percentage of rejection:
(1-(pcauchy(5) - pcauchy(4))) * 100
#No way the rejection percentage is 98.485 for this particular scenerio.

v <- rcauchy(1000)
v <- v[v >= 4 & v <= 5]# No way so this is how the rejection method works. Pretty awesome.
v
v[1:10]

#Using the inversion method, this becomes what is displayed next:
Fa <- pcauchy(4)
Fb <- pcauchy(5)
u <- runif(10, min = 0, max = Fb - Fa)
qcauchy(Fa + u)

#This saves a considerable amount of computation time, especially for large scale simulations.

##Metropolis --- Hastings algorithm:
#Almost all of the previously discussed methods worked with either inversion (if the inverse of the distribution function is known or determined by numerical integration) or rejection sampling. These methods have in common that i.i.d random numbers are simulated. 

#The main goal is as in the previously discussed methods, to simulate random numbers from a theoretical distribution. With the Markov chain Monte Carlo methods, we cannot simulate i.i.d. random numbers but correlated variables of a Markov chain. The violation of the i.i.d. assumption is often taken into account to solve more difficult problems. The most common sampling method for the Markov Monte Carlo chain is the Metropolis Hastings algorithm.

## A few words on Markov chains:

##simple random walk Markov chain:
n <- 10; set.seed(123)
x <- numeric(n)
for(i in 2:n){
	x[i] <- x[i - 1] + rnorm(1)
}
x

#the markov kernel K(X^(t), X^(t+1)) is equivalent to a N(X^(t), 1) density:
set.seed(123)
x <- numeric(n)
for(i in 2:n){
	x[i] <- rnorm(1, mean = x[i - 1])
}
x

#A Markov kernel can always be formulated. From the previous example, we see that the representation with a Markov kernel is equivalent to an out defined Markov chain. We also can observe that the next state (value) only depends on the previous state (value). 

#A stationary stochastic process is given when a probability function exists with the following condition: if X^(t) ~ f, then X^(t+1) ~f. 

#From the existence of a stationary process it follows that, independently from the starting value, a sample from the stationary distribution is generated as long as the period is long enough. Later this will be noted as the burn-in-phase the MCMC needs a certain amount of time to produce values from a stationary distribution, thus the first values belonging to the burn-in phase will be deleted. 

#Also, each "region" can be achieved with positive probability for the defined region. And with an infinite imaginary Markov chain, one can get to any point again (reoccurrence). Compare our example in the first section of this chapter with the drunken person that goes home from the pub. He will come back to the pub with positive probability. 

#"As long as the recurrence of a chain is given, the MCMC converges. The question later on is just how fast the MCMC will converge. In other words, an MCMC often takes a long time to converge, so it is always necessary to evaluate the convergence speed. 

#As for rejection sampling, the aim of the next method is to find a way to generate samples from a target density f. However, as mentioned earlier, MCMC methods have a disadvantage: the i.i.d. assumption is violated since the next simulated value always depends on the previous value. In other words, the samples are correlated. In addition, over a long time, the simulated values do not follow the wanted distribution, that is, the initial samples way follow a very different distribution. Therefore a burn-in period is needed (thus involving a practicioner to throw away the first 1000 samples). 

#People use the MCMC method because the rejeciton method suffers from the curse of dimensionality, since the probability of rejection increases exponentially with the number of dimensions. Since the following method doesn't suffer from the same problem to such an extent, the MCMC method is the only way to simulate high dimensional statistical models.

#Note that auto-correlation can be reduced by thinning the simulated sequence of random numbers. This means that after the burn-in phase, every 10th simulated value or so is kept. However, this also means that a very large number of samples will be needed.

#the Metropolis-Hasting algorithm is described as follows:
	#To learn about the inner works of the algorithm look at page 120.
	
#Through a Markov kernel K with station stationary distribution f, a Markov chain (X^(t), t = 0,1, ...) is generally so that the limit distribution of (X^(t)) is equal to f. The difficulty is in finding a kernel K.

#Example using the Rayleigh distribution (which is used to model lifetimes):
#For the proposal distribution, an x^2 distribution is selected. this distribution is just as skewed as the Rayleigh distribution. For a list of the algorithm used look at page 122.

#The following R code shows the Rayleigh simulation method:
f <- function(x, sigma){
	if(any(x < 0)) return(0)
	stopifnot(sigma > 0)
	return((x/sigma^2) * exp(-x^2/(2*sigma^2)))
}

#In the following simulation, a Rayleigh (4) is sampled by means of the proposal distribution:
i <- 2
xt <- x[i-1] <- rchisq(1,1)
y <- rchisq(1, df = xt)

#And, for every y, r(x_(i-1), y) is calculated. We summarize all this in the following function:
rrai <- function(n = 10000, burnin = 1000, thin = 10, sigma = 4, verbose = TRUE){
##raileight density
f <- function(x, sigma){
	if(any(x < 0)) return(0)
	stopifnot(sigma > 0)
	return((x/sigma^2) * exp(-x^2/(2*sigma^2)))
	}
	x <- numeric(n)
	x[1] <- rchisq(1, df = 1)
	k <- 0; u <- runif(n)
	for(i in 2:n){
		xt <- x[i-1]
		y <- rchisq(i, df = xt)
		num <- f(y, sigma) * dchisq(xt, df = y)
		den <- f(xt, sigma) * dchisq(y, df = xt)
		if(u[i] <= num/den){
			x[i] <- y 
		} else {
			x[i] <- xt 
			k <- k + 1 #y is rejected
		}
	}
	if(verbose) cat("acceptance rate:", (k-burnin)/n/thin, "\n")
	##burn-in:
	if(burnin > 1) x <- x[(burnin+1):length(x)]
	## thinning:
	return(x[seq(1, length(x), thin)])# to tenth value of every loop. 
}

r <- rrai(n = 10000, thin =1, burnin = 1)# what's going on? the length for this object is recorded as 0 in the R console. Will need to see if this is correct or did I do something wrong with the algorithm.
## acceptance rate: 0.4055
r <- rrai(n = 10000, thin = 10, burnin = 1000)
##Acceptance rate: 0.03021 
length(r)# Interesting the length of this experiment is still recorded as 9000. This is different from the author's result what was recorded at 900. 

#We see that the acceptance rate is only about 40 percent without thinning and skipping the first values, and about 3 percent with thinning (thin =10) and the burnin-phase.

#to see the generated sample as a realization of a stochastic process, we can plot it against the index as a polygon line (note that qplot is a short for ggplot). The following graphic shows the simulated random numbers related to the sequence of generation (index).
library(ggplot2)
qplot(1:length(r), r, geom = "line", xlab = "", ylab ="random numbers from Rayleigh(4)")
# again this graphic is different from what the author obtained in the realm of generated indexes. The author had 900 while I obtained 9000. Again I really need to see if I got something wrong with the algorithm function code. 

#I found out what was wrong with the algorithm I forget to code in the thinning argument. 
qplot(1:length(r), r, geom = "line", xlab = "", ylab ="random numbers from Rayleigh(4)")

#The simulated random numbers can also be compared with theoretical quantiles to evaluate whether they follow a Rayleigh distribution. We use the Q-Q plot for this. If the points follow a line, we can expect that our simulated sample is drawn from a Rayleigh distribution. As we see from the following plot, this should be the case. To generate this figure, theoretical quantiles of the Rayleigh distribution, as well as the empirical quantiles, are calculated and plotted against each other:
a <- ppoints(length(r))
sigma <- 4
QR <- sigma * sqrt(-2 * log(1-a)) #Quantiles of Rayleigh
Q <- quantile(r, a)
qqplot(QR, Q, mean ="", xlab = "Rayleigh quantile", ylab = "sample quantile")

##The Metropolis sampler:
#The Metropolis sampler is a sampler version of the basic algorithm since it covers the special case of the distribution to generate being symmetric.
#If we consider a symmetrical distribution, q(X|Y) = q(Y|X), the acceptance probability only depends on the ratio f(Y_t)/f(X_t).

##Example using a symmetrical distribution:
#First, we want to simulate random values from a Beta distribution with the Metropolis-Hastings algorithm ( we have done this already with the rejection method). The density that is considered to simulate it again is B(2,2) and B(1,2). A candidate for a proposal density q is U(0,1). This algorithm is implemented in the following code using the function parameters n (the number of simulated values), burnin (the burn in phase), thin (thinning), cand (candidate distribution), target (target distribution), shape1 (shape parameter 1), and shape2 (shape parameter 2):
mh <- function(n = 10000, burnin = 1000, thin = 10, cand = runif, target = dbeta, shape1 = 2, shape2 = 2){
	if(burnin >= n) {
		stop("burnin is larger than the number of simulations")
	}
	x <- rep(cand(1), n) #initialization 
	for(i in 2:n){
		y <- cand(1)
		rho <- target(y, shape1, shape2)/
						target(x[i-1], shape1, shape2)
		x[i] <- x[i-1] + (y - x[i-1]) * (runif(1) < rho)
	}
	#burnin:
	x <- x[(burnin+1):n]
	return(x[seq(1, length(x), thin)])
}

par(mfrow = c(1,2))
plot(density(mh()), main = "", xlab = "")
plot(density(mh(shape1 = 1)), main = "", xlab = "")

#With the previous function, it is now easy to simulate random numbers from other distributions> Let's do it for the Gamma distribution, specially random numbers from G(2,4). A candidate for a density q is N(1,2):
rgamma <- mh(cand = rnorm, target = dgamma)

#All the given examples according to the Metropolis-Hastings algorithm can be easily carried out with regjection sampling and it should be made with rejection sampling since the disadvantages of non i.i.d. samples is preventaed with rejection sampling. We took these two examples to show how a Metropolis-Hastings works.

##the Gibbs sampler:
#The gibbs sampler also belongs to the class of MCMC methods. It can be seens as a one-step Metropolis-Hastings algorithm where every value is accepted. The gibbs sampler can be perfectly used for sampling from alarge set of variables/parameters by sampling each variable/parameter in turn.

##The two-phase Gibbs sampler:
#Let's write a program for a 2-phase Gibbs sampler to simulate a bivariate normal distribution.
gibbs_bivariate <- function(n = 1000, rho = 0.9, start = 0, burnin = 100, thin = 1){
	x <- y <- numeric(n)
	s<- 1 - rho^2 
	x[1] <- start # to show effect of burnin 
	for(t in 1:(n-1)) {
		y[t + 1] <- rnorm(1, rho*x[t], s)
		x[t+1] <- rnorm(1, rho*y[t+1], s)
	}
	s <- seq(burnin+1, n, thin)
	return(cbind(x[s], y[s]))
}

par(mfrow = c(1,3))
set.seed(123)
##bad start:
b0 <- gibbs_bivariate(n = 200, start = 30, burnin = 0)

##plot the results:
plot(b0, rypw = "o", xlab = "x", ylab = "y", main = "burnin 0", cex.main = 1.3, cex.lab = 1.3)
set.seed(123)
plot(b0[20:200,], type = "o", xlab = "x", ylab = "y", main = "burnin 20", cex.main = 1.3, cex.lab = 1.3, col = grey(10:200/200))
set.seed(123)
plot(b0[20:200,], pch = 20, xlab = "x", ylab = "y", main = "burnin 20", cex.main = 1.3, cex.lab = 1.3)

##The multiphase gibbs sampler:
##Application in linear regression:
#The gibbs sampler is often used in the context of regression analysis and the fit regression parameters. A simple example that shows the basic principles of Gibbs sampling in the regression context is shown next.

#This code for this simple regression problem is as follows. The parameter alpha, beta, and tau define the starting values:
lreg <- function(y, x, time, alpha = 0, beta = -2, tau = 1, burnin = 0, thin = 1){
	n <- length(y)
	##Alpha, beta, tau defining variables
	res <- matrix(, ncol = 3, nrow = time)
	for(i in 1:time){
		alpha <- rnorm(1, mean(y) -beta * mean(x), 1 / (n *tau))
		m <- (sum(x * y) - alpha * n * mean(x)) / sum(x**2)
		s <- 1 / (tau * sum(x ** 2))
		beta <- rnorm(1, m, s)
		w <- y - alpha - beta * x 
		tau <- rgamma(1, ((n / 2) + 1), (sum(w**2)/2))
		res[i,] <- c(alpha, beta, tau)
	}
	s <- seq(1, length((burnin + 1):nrow(res)), thin)
	res <- res[((burnin + 1):nrow(res))[s], ]
	res <- data.frame(res)
	colnames(res) <- c("alpha", "beta","tau")
	return(res)
}

#The following graphic representation shows the results of the Gibbs sampler for the Cars93 data set from the package MASS. The darker the lines in the graphic the higher the iteration, that is, we start off bad but it soon converges to the solutions drawn in black:
data(Cars93, package = "MASS")
set.seed(123)
time <- 100
res <- lreg(Cars93$Price, Cars93$Horsepower, time = time)
par(mar = c(4,4,0.1,0.1))
plot(Cars93$Horsepower, Cars93$Price, pch = 20, xlab = "Horsepower", ylab = "Price", type = "n")
range <- 1 - sqrt(1:time/time)
range <- range + 0.1
#range <- range/sum(2*range)
for(i in 1:time){
	abline(a = res[i, 1], b = res[i,2], col = gray(range[i]))
}
abline(a = res[i,1], b = res[i, 2], col = "red", lty = 2, lwd = 3)
points(Cars93$Horsepower, Cars93$Price, pch = 20)

#We've already done some simple diagnostics for random numbers simulated by the bivariate Gibbs sampler as well as for our example in regression. In addition, we will now plot the simulated values against the index (for our regression problem); take a look at the following graphic. We start again badly for demonstration issues. We observe that a short burn-in phase and fast convergence is indicated. 
set.seed(123)
g <- lreg(Cars93$Price, Cars93$Horsepower, time = 500)
g1 <- cbind(g, "index" = 1:nrow(g))
g1 <- reshape2::melt(g1, id = c("index"))
ggplot(g1, aes(x = index, y = value)) + geom_line() + facet_wrap(~variable, scales = "free_y")

#We clearly see that the Gibbs sampler suffers from auto_correlation for different lags. To get closer to an i.i.d. sample, thinning should be done with a thinning of approximately 15 or larger. This would result in the need to simulate more random numbers (15 times or larger). 
set.seed(123)
g <- lreg(Cars93$Price, Cars93$Horsepower, time = 2000, thin = 15)
plot(acf(g))# This seems like I have insufficient amount of random numbers will need to increase the time argument some more.

g <- lreg(Cars93$Price, Cars93$Horsepower, time = 500, thin = 15)
plot(acf(g))

#Next, we run M chains (M = 5 here) with a larger sequence at different starting values. After basic plotting, we convert them to mcmc objects (with the package coda), since the Gelman-Rubin method as well as the diagnostic plot for Brooks and Gelman is easy to produce with such objects. We allow relatively bad starts --- this is not necessary and a start with rnorm(1, 0, 1) would be better, but we want to see more about the burn-in phase just for demonstration issues:
library("coda")
time <- 2000; M <- 5
set.seed(12345)
df <- lreg(Cars93$Price, Cars93$Horsepower, time = time)
for(i in 2:M){
	df <- rbind(df, lreg(Cars93$Price, Cars93$Horsepower, time = time))
}
df$M <- factor(rep(1:M, each = time))
df$index <- rep(1:time, M)
df <- reshape2::melt(df, id = c("M", "index"))
ggplot(df, aes(x = index , y = value, group = M, color = M)) + geom_line(alpha = 0.5) + facet_wrap(~variable, scales = "free_y")
#The following graphic illustrates 5 independently drawn chains. The burn-in phase seems to be short.

#Brooke-Gelman:
g1 <- list()
M <- 15
set.seed(12345)
for(i in 1:M){
	g1[[i]] <- lreg(Cars93$Price, Cars93$Horsepower, time = time) 
}
g1 <- lapply(g1, function(x) mcmc(as.matrix(x)))
gelman.diag(g1, autoburnin = FALSE)
#Potential scale reduction factors:
#Multivariate psrf 
summary(g1)
#the results give us the median potential scale reduction factor and its 97.5 percent quantile. We also get a multivariate potential scale reduction factor that was proposed by Brooks and Gelman. The function gelman.plot shows the evolution of Gelman and Rubin's shrink factor as the number of iterations increases. Note that if autoburnin is set to TRUE, then the first half of simulated random numbers are removed. 
gelman.plot(g1, autoburnin = FALSE)
#We see that the shrink factor converges to almost 1 for longer iterations. This might be an indication that the burn-in should be kept longer. 

#with all these fundings, we can conclude that a good chain would be as follows:
burnin <- 1000
time <- burnin + time *20
g <- lreg(Cars93$Price, Cars93$Horsepower, time = time, burnin = burnin, thin = 20)

##The evaluation of random numbers --- an example of a test:
#The presented test is a typical test for random number generators. It is based on evaluating the distribution of counts.

#For the random numbers generated on a circle with radius 1 (say big_circle), a large number of circles with radius r can be defined. The following function result is a logical vector that indicates if points lay in a circle that is fully within the big_circle.
circle <- function(x, r = 0.05){
	repeat{
		x1 <- runif(1, -1, 1)
		x2 <- runif(1, -1, 1)
		if(sqrt(x1^2 + x2^2) <= (1 - r)) break
	}
	inCircle <- ((x[,1] - x1)^2 + (x[,2] - x2)^2) <= r^2
	return(inCircle)
}

#The number of points in the circle should have a poisson distribution with the mean r^2. Thus we count for each circle the number of points and create a test for the counts. 

#the following figure should explain the cutting of circles visually:
set.seed(123)
##take possible radii
x <- matrix(runif(10000, -1, 1), ncol = 2)
##radii to the square 
r2 <- rowSums(x^2)
## r2 smaller than 1 are kept
x1 <- x[r2 <= 1,]
par(mar = c(2,2,0.1,0.1))
plot(data.frame(x1), pch = 20)
for(k in 1:8) points(data.frame(x1[circle(x1, 0.2),]), col = k, pch = 20)

#We repeart the cutting of circles 2,000 times and count for each circle the number of points included:
set.seed(123)
z <- replicate(2000, sum(circle(x1)))
#Tabulated, this yields the following values:
TAB <- table(z)
TAB

#This means that in this simulation, once there was just one observation inside one specific circle. Most circles have around 9 and 10 values included, that is, the mode is 9 and the median count is as follows:
laeken::weightedMedian(as.numeric(names(TAB)), as.numeric(TAB))

#However, the variance might be high. We now want to test whether extreme events occur by chance. The observation counts may also be visually compared with the theoretical counts of a poisson distribution. 
lambda <- nrow(x1) * 0.05^2
PROB <- dpois(as.numeric(names(TAB)), lambda)
b <- barplot(TAB/length(z))
points(b, PROB, col = "red", pch = 16)

#We now want to reduce to 6 classes and assign our simulated values based on these classes:
#Five classes:
QP <- qpois(seq(0,1, by = 1/6), lambda)
QP
#frequency counts in those classes 
TAB1 <- table(cut(z, QP, include.lowest = TRUE))
TAB1

#Since we do not know the population and have only one sample (our simulation) simulated, we must take the uncertainty into account and apply a statistical test. We use the x^2 goodness of fit test. Next, the theoretical quantiles and the theoretical class widths are calculated,which exactly express the probabilities to fall into the classes:
ppois(QP, lambda)
QP1 <- QP
QP1[1] <- -1
PROB1 <- diff(ppois(QP1, lambda))
PROB1
chisq.test(TAB1, p = PROB1)

#We are not in the rejection region, and thus the null hypothesis --- the number of points in the circles with radius r^2 is Poisson distributed -- cannot be rejected. Thus, the Mersenne-Twister random number generator has passed this test. 