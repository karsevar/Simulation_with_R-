### Chapter 6 Probability theory Shown by simulation:
##Some basics on probability theory:
#Stochastic theory in turn uses probabilistic concepts -- randomness and laws regarding randomness -- for the modeling and analysis of real random processes (for example in economic forecasting). 

#A random process or random experiment is any procedure that can be infinitely repeated and has a well-defined set of possible outcomes. For example, rolling the dice is a random experiment.

#Interesting the probability outcomes for a document phenomina is denoted by the symbol of omega = {the number of random outcomes}. In this case, omega (equals a die) and the number of outcomes is {1, 2, 3, etc.}.
#A random variable, X = {X_1 . . . X_k}, can take on a set of possible different values each with an associated probability.

#The output of the random experiment is a random variable. Example, even numbers on a die.

#An event A: the experimental output, x, has a specific property, A. Events are therefore subsets of omega. In our dice example, event A, throw an even number, means the quantity {2,4,6}.

#The probability, P(A) is the probability that a particular event A occurs. Example: For the event A, "toss the head of a coin", the probability is P(A) = 1/2.

##Probability distributions:
#We take a small excursus to probability distributions, since they are used frequently in the book. The theoretical distributions are of central importance:
	#To approximate functions, such as a discription of empirically observed frequency distributions in descriptive statistics.
	#The determination of probabilities for results of certain random experiments in mathematical statistics.
	
#Some important theoretical distributions are, for example, the binomial distribution, the Poisson distribution, the hyper-geometrical distribution, the uniform distribution, the exponential distribution, the normal distribution, the X^2 distribution, and the t distribution. 

##Discrete probability distributions:
#Married or not married, in the Austrian population, defines already a discrete probability distribution.
#One calls the function that assigns each elementary event, j, its probability, p_j the probability function of the observed distribution. It is as follows:
	# For a mathematical overview of discrete probability theory and the mathematical formula that goes into it, make sure to consult page 174- 175 in chapter 6. Currently I'm unable to understand what the author is attempting to convey since I don't have to proper knowledge. 
	
#As an example of a discrete probability distribution, we name Binomial distribution.

#A Bernoulli model consists of a sequence of Bernoulli-experiments (binomial distribution where n = 1), with the following conditions:
	#For each trial, only two outcomes (events) are possible, say A and A_upper score. 
	#Probabilities for A and A_upper score are a constant of all experiments, P(A) = p and P(A_upper score) = 1 - p. In summary, n replications will be done and the individual experiments are independent. The frequency h_n(A).
	#(so in other words this formula pretty much shows the inner workings of the function that calculates density or rather categorical probability when working with discrete variables).
	
##Continuous probability distributions:
#For the mathematical description or rather overview of continuous probability distribution please consult page 175. The vocabulary and the mathematical concepts are still too advanced for me to comprehend much less reproduce.

#As a representative of a continuous probability distribution, we mention the normal distribution. Normal distribution is the most important distribution in mathematical statistics. Not many technical and biological data follow an approximately normal distribution, but estimated parameters do. Normal distribution is the basis of many estimation and testing methods.

##Winning the lottery:
#This following experiment will be ran with the dataset from the British lottery numbers over the past seven months. 
library(ggplot2)
library(RCurl)
URL <- "https://www.national-lottery.co.uk/results/euromillions/draw-history/csv"
lotto <- read.csv(textConnection(getURL(URL)))

str(lotto)# good it seems that the data set is fairly small. 51 total observations with 10 variables. I'm curious what the these variables might explain about the data. Now I understand the variables are the different lottery games in the UK, very interesting indeed.

# Let us see if some numbers are drawn more frequently. The following graphic shows a bar chart of these frequencies.
numbers <- unlist(c(lotto[,2:5]))# this following line of code narrows down the lottery games that will be plotted to only the Ball.1 through Ball.5. I find it interesting that he didn't include the lucky.star.1 and lucky.star.2. 
numbers

qplot(factor(numbers), xlab = "Geinnzahlen") + theme_bw() + theme(axis.text.x = element_text(angle=90)) + scale_y_continuous(breaks = 0:10)

#People who know some of the basics of probability theory would rather think of a uniform distribution regarding the frequencies of drawn lottery numbers. But when looking at the following example, can we really believe this is true? Can we believe that the probability of a number being drawn in the British lottery is 1/50?
#By no means! The number of draws is just too small. But can we believe in this when the number of draws will be increased?

##The weak law on large numbers:
#The weak law of large numbers is applied to betting offices, in financial assessments and for insurance, and so on. It builds the foundation of statistics, and data scientists should be awar of it. By understanding the weak law of large numbers and the central limit theorem, one understands the basics of mathematical statistics.

##Emperor penguins and your boss:
#(important question to consider) A very interesting question is to ask what happens when we have more and more random variables available, when we increase the number of random variables over and over? What happens if n equals infinity. The limit theorems provide evidence on this.

##Limits and convergence of random variables:
#The probability that random number are outside this interval gets smaller and smaller when n trends to infinity. In other words, the limit value of this probability is zero. We say also: the sequence X_n converges in probability to a. 
#For a refresher on convergence and divergence consult chapter 3.

##Convergence of the sample mean -- weak law of large numbers:
#The larger the sample, the smaller the variance of the estimate and the smaller the uncertainty.
#The law of large numbers tells us that the sample mean converges to the true mean, but in a special way: in probability. 

##Showing the weak law of large number by simulation:
#consider the simplest of all simple examples, the toss of a coin. We want to flip a coin again and again and we surely agree that the coin is fair, when the observed probability of tossing a head (or number) is approximately 0.5, and if the next outcome of the next throw is not dependent on the previous result (independence). This can also be expressed with the binomial distribution. 
dbinom(x = 0, size = 1, prob = 0.5)# the result was as expected, 0.5.

#We want to perform the random experiment throw the coin n times and we evaluate the results. In R, we can use either the funciton sample or the funciton rbinom to simulate a coin toss. For one throw of a coin the code looks like this:
sample(c("head","tail"), size = 1)
rbinom(n = 1, size = 1, prob = 0.5)

#We write a function which simulates the tosses of a coin and which evaluates the probabilities of head ater n experiments, as well as calculating the abolute error to the true probability. The nice thing about this experiment is that we know the truth (P(A) = 0.5). this is not the case in practice.
simCoin <- function(n, p = 0.5, repl = 1){
	stopifnot(n > 0 | !is.vector(n) | p < 0 | p > 0 | !is.vector(repl))
	##function for one simulation
	r <- function(){
		res <- rbinom(n, 1, p)
		tosses <- 1:n
		pA <- cumsum(res) / 1:n
		abserror <- abs(pA - p)
		return(data.frame(res = res, tosses = tosses, pA = pA, abserror = abserror))
	}
	##simulate
	df <- r()
	if(repl > 1){
		for(i in 2:repl){
			df <- rbind(df, r())
		}
	}
	##return
	df$repl <- rep(1:repl, each = n)
	ll <- list(res = df$res, tosses = df$tosses, pA = df$pA, absfehler = df$abserror, repl = as.factor(df$repl))
	class(ll) <- "Coin"
	return(ll)
}
##print
print.Coin <- function(x, ..., s = NULL){
	if(!is.null(s)){
		cat("After", s, "random draws: the estimate P(A) =", x$pA[s], "\nand the absolute error", x$absfehler[s], "\n")
	} else {
		m <- max(x$tosses)
		cat("After", m , "random draws: the estimated P(A) =", x$pA[m], "\nand the absolute error", x$absfehler[m], "\n")
	}
}

#The first n = 10 tosses of the coin:
#for reproducibility:
set.seed(1234)
#10 throws
simCoin(10)
#the results say that after 10 random draws: the estimated P(A) = 0.7 and the absolute error is 0.2. Really neat that this function actually worked much less the returned value from function simCoin was sent to the function print.Coin, I really have a long way to go with regards to R programming and statistical computing. 

#So if we throw only 10 times, the error might be very large. We expect that the error gets smaller when we throw more often (increases the sample size n):
sim <- simCoin(5000)
print(sim, s=100)# Interesting, make sure to not assume that the value in the book should be the same value that you get in your individual computations since the characteristics for this experiment are random. This means that the P(A) value and the absolute error hovers around 0.52 - 0.38 and 0.11 - 0.02 respectively. 

print(sim, s = 1000)
print(sim, s = 5000)

#What we already see is that the larger n, the closer the estimated value (ratio of head to number of throws) and the true value/expection.

#by visualizing the results we gain even more knowledge. We first define a plot function:
plot.Coin <- function(x, y, ...){
	df <- data.frame(res = x$res, tosses = x$tosses, pA = x$pA, repl = x$repl)
	if(length(unique(df$repl)) == 1){
		ggplot(df, aes(x = tosses, y = pA)) + 
		geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1)) +
		theme(legend.position = "none")
	} else if(length(unique(df$repl)) > 10){
		gg <- ggplot(df, aes(x = tosses, y = pA, group = repl)) +
			geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1))
		##add median line and confidence interval.
		dfwide <- reshape2::dcast(df, tosses ~ repl, value.var = "pA")
		dfwide <- dfwide[, 2:ncol(dfwide)]
		med <- apply(dfwide, 1, median)
		q025 <- apply(dfwide, 1, quantile, 0.025)
		q975 <- apply(dfwide, 1, quantile, 0.975)
		stat <- data.frame(med = med, q025 = q025, q975 = q975, n = 1:max(x$tosses), repl = max(as.numeric(df$repl)))
		gg +
			geom_line(data = stat, aes(x = n, y = med), color = "red", size = 1)+
			geom_line(data = stat, aes(x = n, y = q025), color = "orange", size = 0.7) +
			geom_line(data = stat, aes(x = n, y = q975), color = "orange", size = 0.7) +
			theme(legend.position = "none")
	} else {
		ggplot(df, aes(x = tosses, y = pA, color = repl)) +
			geom_line() + geom_abline(intercept = 0.5) + ylim(c(0,1))
	}
}

plot(sim)
#After running the following line of code, the coin is tossed 5000 times in total and the observed probability (frequency of heads divided by the number of throws) is plotted as a line. A line is not quite correct because the number of throws is discrete. We do not want to be too critical because this representation gives a smoother image of the overall mechanism behind the weak law of large numbers. 

#We see that:
	#Apparently, the observed probability converges to the expected value.
	#Changes in the observed probability according to the step n to n = n +1 are getting smaller with increasing n. If the coin has already been thrown 1000 times a new throw of the coin hardly alters the observed probability at all.
	
#We see intuitively that the law of large numbers should work. We want to repeat the experiment at first 10 times. We toss the coin 10 by 5000 times and show the result in the following graphic.
set.seed(1234)
sim <- simCoin(n = 5000, repl = 10) 
plot.Coin(sim)# Now I understand the repl variable is actually the number of experiments the function will carry out. And so this function says that it will run 10 experiments of 5000 tosses each. 

#However, we can observe that not every line (simulation of 5000 coin tosses) converged to 1/2 after 5000 tosses. Is there something wrong? Not at all. It's just because we took only 5000 tosses. We can also show that the law of large numbers works correctly when repeating the simulation more often and taking the median of it as well as certain quantiles on the tails. In the mean, the curves must converge to 1/2 and the quantiles must become smaller with increasing sample size. 

#Let us repeat the simulation 1000 times; we toss the coin repeatedly, 1000 times 5000 throws each. The result is shown with the following graphic.

sim <- simCoin(n = 5000, repl = 1000)
plot(sim)# What an awesome representation. Now I understand the plot.Coin() function created a plot that calculated the median and the error bounds of the 1000 trials for each toss. Very cool function. I might have to reverse engineer it out of curiousity. 

#What can we know?
	#We recognize that the weak law of large numbers works, even if we cannot perform the experiment infinitely often. The mean curve converges soon to 1/2 (We also observe that our random number generator works well.)
	#The dispersion decreases with increasing sample size, but not linearly with the sample size. Doubling the sample size implies not a reduction of uncertainty by a factor of 1/2. The standard deviation decreases by sqrt(n) (compare the formula for the preceding variance).
	
#In the graphic, we have the 1000 results for each toss and we averaged (the median, red line) them for any number of throws. Also the 2.5 and 97.5 percentile are calculated and shown in the figure's (orange lines). this provides information about the decrease in the scattering around the expected value of 1/2.

#As already mentioned, we cannot indefinitely perform a random experiment on the computer. Decreasing variance with increasing sample size can also, for example, be shown with the theoretical binomial distribution B(n, p = 0.5). For different n this is shown in the following figures.

plotbinomcoin <- function(n){
	plot(0:n/n, dbinom(0:n, n, 0.5), type = "h", xlab = paste("relative frequencies (n = ", n,")"), ylab = "p")
}
par(mar = c(4,4,0.5,0.5), mfrow = c(4,2))
plotbinomcoin(10)
plotbinomcoin(20)
plotbinomcoin(40)
plotbinomcoin(80)
plotbinomcoin(160)
plotbinomcoin(320)
plotbinomcoin(5000)
plotbinomcoin(10000)
#Very interesting representation. I will love to remember this fairly simplistic illustration of this statistical principle.

#What does the figure show? When looking at the upper left graphics, it shows that theoretical probabilities for 10 tosses. For example, the probability is about 0.05 to toss two heads out of 10 tosses of a coin. The example in the following graphic also shows, in turn, that the deviations of the observed probabilities to the expected value to be greater than to an arbitrarily small number, with growing n is more and more unlikely. For example the upper graphics we see that the distribution shrinks, largely by increasing the sample size. 

##The central limit theorem:
#The classical theory of sampling is based on the following fundamental theorem.
	#When the distribution of any population has finite variance, then the distribution of the arithmetic mean of random samples is approximately normal, if the sample size is sufficiently large.
	
#The following code is a simulation proving this mathematical fact:

#The following setup is necessary:
	#We draw samples from populations. This means that we know the populations. This is not the case in practice, but we show that the population can have any distribution as long as the variance is not infinite.
	#We draw many samples from the population. Note that in practice, only one sample is drawn. For simulation purposes, we assume that we can draw many samples.
	
#For the purpose of looking at our defined populations (pop = TRUE) and the distribution of arithmetic means from samples of these populations, we define the following function. In the first quarter of the code, the normal distribution is used, followed by the uniform and exponential distribution, and in the fourth quarter of the code, the Beta distribution is used.

clt.Sim <- function(n = 1, reps = 10000, nclass = 16, pop = TRUE, estimator = mean){
	old.par <- par(oma = c(0, 0, 1.5, 0), mfrow = c(2,2), mar = c(4,4,2,0.5))
	on.exit(par(old.par))
	##normal:
	norm.mat <- matrix(rnorm(n *reps), ncol = n)
	norm.mean <- apply(norm.mat, 1, estimator)
	x <- seq(min(norm.mean), max(norm.mean), length = 50)
	normmax <- max(dnorm(x, mean(norm.mean), sd(norm.mean)))
	tmp.hist <- hist(norm.mean, plot = FALSE, prob = TRUE, nclass = nclass)
	normmax <- max(tmp.hist$density, normmax) * 1.05
	hist(norm.mean, main = "normal", xlab = "x", col = "skyblue", prob = TRUE, ylim = c(0, normmax), nclass = nclass)
	lines(x, dnorm(x, mean(norm.mean), sd(norm.mean)))
	##exponential:
	exp.mat <- matrix(rexp(n * reps, 1/3), ncol = n)
	exp.mean <- apply(exp.mat, 1, estimator)
	x <- seq(min(exp.mean), max(exp.mean), length = 50)
	expmax <- max(dnorm(x, mean(exp.mean), sd(exp.mean)))
	tmp.hist <- hist(exp.mean, plot = FALSE, prob = TRUE, nclass = nclass)
	expmax <- max(tmp.hist$density, expmax) * 1.05
	hist(exp.mean, main = "exponential", xlab = "x", col = "skyblue", prob = TRUE, ylim = c(0,expmax), nclass = nclass)
	if(pop) lines(x, dexp(x, 1/3)) else lines(x, dnorm(x, mean(exp.mean), sd(exp.mean)))
	##uniform
	unif.mat <- matrix(runif(n * reps), ncol = n)
	unif.mean <- apply(unif.mat, 1, estimator)
	x <- seq(min(unif.mean), max(unif.mean), sd(unif.mean))
	unimax <- max(dnorm(x, mean(unif.mean), sd(unif.mean)))
	tmp.hist <- hist(unif.mean, plot = FALSE, prob = TRUE, nclass = nclass)
	unimax <- max(tmp.hist$density, unimax) * 1.05
	hist(unif.mean, main = "uniform", xlab = "x", col = "skyblue", prob = TRUE, ylim = c(0, unimax), nclass = nclass)
	if(pop) lines(x, dunif(x)) else lines(x, dnorm(x, mean(unif.mean), sd(unif.mean)))
	##Beta 
	beta.mat <- matrix(rbeta(n * reps, 0.35, 0.25), ncol = n)
	beta.mean <- apply(beta.mat, 1, estimator)
	x < seq(min(beta.mean), max(beta.mean), length = 50)
	betamax <- max(dnorm(x, mean(beta.mean), sd(beta.mean)))
	tmp.hist <- hist(beta.mean, plot = FALSE, prob = TRUE, nclass = nclass)
	betmax <- max(tmp.hist$density, betamax)
	hist(beta.mean, main = "Beta", xlab = "x", col = "skyblue", prob = TRUE, ylim = c(0, betamax), nclass = nclass)
	if(pop){
		lines(x, dbeta(x, 0.35, 0.25))
		mtext(paste("Populations"), outer = TRUE, cex = 1.2)
	} else {
		lines(x, dnorm(x, mean(beta.mean), sd(beta.mean)))
		mtext(paste("sample size =", n), outer = TRUE, cex = 1.2)
	}
}

#First the distribution of our chosen populations is shown (lines) and one realization of those populations each (histogram).
clt.Sim()
# I believe that I will have to change the scaling for this function a little bit. But with that said, this graphic seems like it illustrates the convergence between the population mean and the sample mean. 

#For these populations, we want to draw 10000 samples of size n = 2. For each sample, we then calculate the arithmetic means. Thus 10,000 sample means are obtained. The distribution of the sample means is then visualized, see the following graphic. The lines correspond to the theoretical normal distribution, and the histogram corresponds to the distribution of sample means.
clt.Sim(2, pop = FALSE)

#For sample size n = 2 the distribution of sample means is not normally distributed, except the samples are drawn from a standard normal distribution population. The distribution of a sample means regarding teh Beta distribution is tri-modal. This can be easily explained. Remember that samples of size = 2 are repeatedly drawn from a Beta distribution. It is very likely that either two small values, two large values, or one large and one small value is drawn. The arithmetic mean of a sample of size 2 from a Beta distribution is thus likely to be at the tail or in the center of the distribution of smaple means. No valid inference statistics can be made in a classical manner if the central limit theorem fails as shown here. 

#However, what will change if we increase the sample size from 2 to 10? Except for the exponential distribution population, the sample means are approx normal.
clt.Sim(10, pop = FALSE)

#We easily would observe if we further increase n, for example, n = 30, that all distributions of sample means drawn from these distributions are approximately normal. Moreover, if the sample size is large enough, the sample means are approximately normal independent of how the populations looks (as soon as the variance of the population is finite): the central limit theorem. 

#Of course, in practice one will only draw one sample (and not 10,000) from a population with unknown characteristics. However, if the sample is large enough (say n = 50), we know about the characteristics of an estimator such as the sample mean. This works for almost any estimator, even if highly non-smooth statistics such as the median. Also, the sample medians are approximately normally distributed whenever the smaple size is large enough.
clt.Sim(n = 100, pop = FALSE, estimator = median)

##Properties of estimators:
#Especially in the following chapters, designations such as bias or asymptomic unbiasedness will be used repeatedly. These expressions are used to describe properties of an estimator.
#A point estimate alone contains no information on accuracy. Thus an interval estimator must also be estimated that expresses the uncertainty of the point estimate.

##Properties of estimators:
#It is desirable, for example, that for repeated drawn samples, the sample estimates spread around in the middle of the true parameter (unbiasedness).

#The following terms define the properties of estimators:
	#Unbiasedness 
	#Consistency: when increasing the sample size, the estimator is closer to the population parameter.
	#Efficiency: Unbiased estimator with minimal variance.
	
#There are different methods to find useful estimators for parameters of a distribution, the so-called maximum likeihood method being the most important.

#Other important terms include:
	#Bias 
	#Asymptotic unbiasedness 
	#Mean squared error (MSE)
	
#If no bias is present, the MSE reduces to the comparison of the variances of estimated statistics. In other words, the MSE and the variance are equivalent in this case. (I believe that the author is talking about the difference between small sample size t distributions and the larger sample size distribution t tests. Very interesting, so this is how they conceptualize the property of having to use degrees of freedom for smaller samples and assumed normal distribution for larger samples).

#This all sounds nice, but how do you estimate the bias without knowing the true population parameter? How do you evaluate asymptotic unbiasedness when the population parameter is unknown, as it is the case in practice? And how do you estimate the MSE if the bias is unknown?

#In general we can answer those questions only by simulation studies. In other words, the properties of an estimator are typically evaluated by model-based simulations when the sample is drawn with simple random sampling, and evaluated with design-based simulation studies when the sample is drawn with simple random sampling, and evaluated with design-based simulation studies when the sample is drawn with complex sample designs. 

##Confidence intervals:
#Along with point estimates, confidence intervals are commonly reported to show the reliability of the point estimates. For example, a confidence interval can be used to describe how reliable estimates from surveys are. The average hourly wages of employees taken from a simple random sample survey of 1000 respondents might be 31 Euros. A 99 percent confidence interval for the average income of the population might be 25 to 37 Euros. However, since there is no negative income, the confidence intervals must not be always symmetric, as is the case also for woting-intenitions, for example.

# Generally, from the observed data, a lower and upper bound -- an estimated interval -- for the population parameter is estimated. The population parameter is unknown but fixed (an actual parameter in the population). The estimated interval, however, depends on the observations in the sample and is therefore random. Often, the population parameter is contained therein, but sometimes, it is not. The colculation formula should be such that, for example, 100(1 - significance level) = 95 percent of all samples provide a population parameter covering interval. Increased coverage (a smaller significance level) means the larger the estimated confidence intervals. 

#In a nutshell: we are looking for two estimates, U and O, specifying the limits of the confidence interval for a given coverage of 100(1 - significance level) percent:
	#Important formula: P(U <= population parameter <= O) = 1 - significance interval
	
#Formulas on confidence intervals depend on whether the variance of the population parameter is known or not. For smaller samples, one has to estimate the confidence intervals via a t-distribution instead of the standard normal distribution. However, if the sample size is sufficiently large, the t distribution is approximately equal to the normal distribution. 

#Funny fact about the t-distribution. The technique was discovered by William Sealy Gosset (a brewer at the Guinness brewery).

#Let's take a small exercise. We use the income variable of the Prestige data and estimate the arithmetic mean and confidence interval:
library(car)
data("Prestige")
m <- mean(Prestige$income)
m
P <- dim(Prestige)[1]
se <- sd(Prestige$income) / sqrt(P)
tval <- qt(0.975, df = P - 1)
cat(paste("KI: [", round(m - tval * se, 2), ",", round(m + tval * se, 3), "]"))

#Let's come back to explanations of the confidence interval. Let's draw 10 samples of size n = 50 each from a exp(1)-distribution. Each sample leads to a different estimation of the mean and to another estimated interval. How many intervals contain the true value of the mean = 1?

set.seed(11112)
alpha <- 0.05
normval <- qnorm(1 - alpha/2)
numsamp <- 50; numsim <- 10
normmat <- matrix(0, nrow = numsim, ncol = 2)
y <- 1:numsim; ymat <- rbind(y, y)
for(i in 1:numsim){
	samp <- rexp(numsamp) #generate random exponentials 
	sampmean <- mean(samp)
	sampse <- sqrt(var(samp)/numsamp)
	normmat[i,] <- c(sampmean - normval * sampse, sampmean + normval * sampse)
}

matplot(t(normmat), ymat, pch = " ", yaxt = "n", ylab = "", xlab = "confidence intervals") #empty plot 
matlines(t(normmat), ymat, lty = rep(1, numsim), col = 1)
abline(v = 1)

#We can observe that only eight of the estimated intervals cover the true population parameter indicated by a vertical line. Does that mean that the confidence interval was not properly specified? Not at all.
#Were one to repeat the procedure described more often, in average 95 out of 100 unrealized intervals should contain the true value of mean = 1 of (given significance interval = 0.05).
