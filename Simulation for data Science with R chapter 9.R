### Chapter 9 The EM Algorithm 
#The Expectation Maximization algorithm is actually not really an algorithm, but a procedure for algorithms for the computation of the maximum likelihood estimators in data with missing values. The EM algorithm is typically used for problems where no closed form solution is known; that is to say for the special kind of optimatization problems where iteration is the only chance to get close to the optimal solution.

##The basic EM algorithm:
#Introduction on likelihood and maximum likelihood using the coin flip exercise. 
#The solution is often much easier when we use logarithms. Thus, in our case, we want to maximize the log likelihood dnoted by 1(population parameter).
		#argmax_population parameter l(population parameter)= argmax_population parameter log(population parameter^7 (1 - theta)^3)
		
#Again we take the derviative and set the derivative to zero to obtain the maximum:
		#To look at the formula examples for this exercise make sure to check out page 303. 
		
##Formal defininition of the EM algorithm:
#Let's first motivate the EM algorithm, according to its initial formalation as an algorithm, to estimate missing values in a data set.

#The EM algorithm is an iterative method that alternately performs two steps: the E_step (= Expected step) and the M-step (=Maximization step). In this method, the likelihood function L{population parameter, X_obs, X_Miss) is maximized related to population parameter, which is equivalent to maximizing the log-likelihood function.
	
##Introductory example for the EM algorithm:
#In the following example, we want to explain the EM algorithm based on a simple 2 by 3 table with one missing cell:
y <- matrix(c(11,22,16,24,17,NA), nrow = 2)
y

#What we always have to do in the EM algorithm is choose starting values. Thus we initialize the missing valu, for example, we may choose the arithmetic mean of the observed values of y for initialization:
m <- mean(y, na.rm = TRUE)
m
y[2,3] <- m
y#I believe this is used to fill in the blank cell that was left through setting the mean() function to na.rm = TRUE.

#Next we start the iteration. We iteratively impute the missing value x_23 with plausible values using the following E and M steps:
##stop criterion 
eps <- 0.001
##Initialisations
yalt <- y
n <- 0
converged <- FALSE
##iteratiion 
while(!converged){
	n <- n + 1
	yalt <- y
	ml <- mean(y)
	##E_step (estimate parameters)
	a <- rowMeans(y) - ml
	b1 <- colMeans(y) - ml
	##M-step (update y23)
	y[2,3] <- ml + a[2] + b1[3]
	##stop criterion 
	converged <- (abs(y[2,3] - yalt[2,3]) < eps)
}

list(yImp = y, iterations = n)

#We see that we needed 21 iterations to achieve the final result of the imputed value of X_23. We'll come back to the issue of imputation soon, but first we'll discuss another application of the EM algorithm.

##The EM algorithm by example of k-means clustering:
#probably the most famous algorithm for clustering observations to groups is the k-means algorithm. We will see that this algorithm is just a variant of the EM algorithm.

#The k-means algorithm can be implemented as follows. Fix n_c, 2 <= n_c <= n, and choose the termination tolerance variance > 0, 0.001. Initialize U^(0) (for example randomly). 

#Pages 305 through 306 has some very good examples and source equations for the k-means clustering EM algorithm. I currently don't have the experience to understand what the author is getting at. Will need to go back to this example once I obtain some proficiency in statistics. 

#We define the whole algorithm of k-means in the following. This is just to get in touch with the code and algorithm. For professional implementations of the k-means, see for example, (Leisch 2006).
#for the cluster algorithm we need a distrance function. We use the Manhattan distance:
distMan <- function(x, centers){
	if(class(x) == "data.frame") x <- as.matrix(x)
	d <- matrix(0, nrow = nrow(x), ncol = nrow(centers))
	##dist center to observations for each cluster 
	for(k in 1:nrow(centers)){
		d[,k] <- abs(ColSums((t(x) - centers[k,])) )
	}
	return(d)
}

#And we need a function that calculates means, for example, we may use the arithmetic mean, but we may also use the median as in the following.
means <- function(x, cluster){
	cen <- matrix(NA, nrow = max(cluster), ncol <- ncol(x))
	##cluster means for each cluster
	for(n in 1:max(cluster)){
		cen[n,] <- apply(x[cluster==n,], 2, median)
	}
	return(cent)
}

#We write a function for the k-means algorithm, which implements the formulas before. As a means to show the EM-approach in detail we will make some plots using for loops.

my_kmeans <- function(x, k, clmeans = means, distance = distMan, iter = 99999, seed = NULL){
	if(!is.null(seed)) set.seed(seed)
	cent <- newcent <- x[sample(1:nrow(x), size = k),]
	oldclust <- 0
	j <- 0
	for( i in 1:iter){#better: while()
		j <- j + 1
		cent <- newcent
		##M-step
		dist <- distance(x, cent)
		clust <- max.col(-dist)
		##E-step
		newcent <- clmeans(x, clust)
		if(all(clust == oldclust)) break()
		oldclust <- clust 
	}
	res <- list(centers = cent,
				cluster = factor(clust),
				iterations = j)
	return(res)
}

#As we can see, in k-means clustering the E-step is the fitting step and the M-step is the assignment step. iterating the E and M-step iteratively improves the solution. This means that J(X,V, U) gets smaller in each iteration. We break the algorithm if the cluster assignment is not changing anymore. 

#We want to keep it simple and we want to show the clusters visually. So we've taken a two-dimensional data set and shown it in the following graphic.
data(Nclus, package = "flexclust")
x <- data.frame(Nclus)
library("ggplot2")
qplot(X1, X2, data = data.frame(Nclus))

#In the following we plot the results after iterations 1,2 and after convergence. Instead of our simplified implementation of the k-means algorithm, we use the default k-means implementation of R. Some variants of the k-means exist, where we chose the algorithm of "MacQueen", but only in order to explore the algorithm (the default method, "Hartigan-wong" is converging too fast to show the steps of the algorithm). Note that the k-means algorithm starts with randomly chosen cluster centers. Thus we have to set the seed to ensure the same starts in each call of the k-means:
set.seed(123456)
c11 <- kmeans(x, centers = 4, iter.max = 1, algorithm = "MacQueen")
set.seed(123456)
c12 <- kmeans(x, centers = 4, iter.max = 2, algorithm = "MacQueen")
set.seed(123456)
c13 <- kmeans(x, centers = 4, iter.max = 3, algorithm = "MacQueen")
set.seed(123456)
c14 <- kmeans(x, centers = 4, iter.max = 4, algorithm = "MacQueen")

#We then plot the results after the E-step and after the M-step for the first two iterations, but also for the final solution. this can be done easily when accessing the cluster centers from the k-means results, for example, for the first colution after one iteration with:
c11$centers
#The solutions after iteration 1,2 and after convergence is now shown in the following graphic on page 311. Will need to recreate the plot later on. The calculated centers (E-step) and the allocation of observations to their nearest clust (M-step) are shown in detail:

#Note that the k-means only takes the centers into account and works with a distance function to calculate the distance from the observations to the cluster centers. Another (more favorable) approach is to incorporate them within the shape of the clusters. this is implemented in the model-based clustering framework. The model-based procedures mostly give better clustering results but they are computationally more complex since in each E-step the covariance of each cluster must be additionally calculated. 

##The EM algorithm for the imputation of missing values:
#The EM algorithm is extensively used for imputation of missing values. In the following we want to show how the EM algorithm works generally for these kind of problems.

library(MASS)
library(robustbase)
library(VIM)
data(sleep)
str(sleep)

#How can we impute missing values in the variable Sleep? This might be done by performing regression fits. Ideally, we alread initialize missing values from a well-performing algorithm for imputation, for example, a k-nearest neighbor imputation approach. However, to see the progress of the EM, we starting very badly regarding the initialization of missing values in the variable Sleep, using the worst initialization just with a large number. Note that we also need the index of the missing values.

##Index of missing values:
ind <- data.frame(is.na(sleep))
##Initialization 
sleep <- kNN(sleep)
##Toverwrite missing initialization with bad choice
sleep$Sleep[ind$Sleep] <- 2240 #bad initialization 
##Initialized missing values in variable sleep
sleep$Sleep[ind$Sleep]

#We then if a model for the first variable. The model results (regression coefficients) are then used to predict the missing values.
##E-step(1)
lm1 <- lm(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, data = sleep)
##M-step (1)
sleep$Sleep[ind$Sleep] <- predict(lm1)[ind$Sleep]
##print of updated missing values 
sleep$Sleep[ind$Sleep]

#We may continue with the second iteration:
##E-step (2)
lm1 <- lm(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, data = sleep)
##M-step (2)
sleep$Sleep[ind$Sleep] <- predict(lm1)[ind$Sleep]
##print of updated missing values 
sleep$Sleep[ind$Sleep]

#We see that the values still change a lot. Let's do this iteratively until the values will not change more than within a very small threshold. We can write a small function for the imputation of the variable Sleep:
EMregSleep <- function(method = lm, eps = 0.001, init = "bad"){
	##index of missing values 
	ind <- is.na(sleep)
	colnames(ind) <- colnames(sleep)
	indsleep <- ind[,"Sleep"]
	##initialization 
	if(init == "bad"){
		sleep <- kNN(sleep, imp_var = FALSE)
		sleep$Sleep[indsleep] <- 2240 #bad initialization 
	}
	if(init == "worst"){
		sleep[ind] <- 2240 #worst initialization 
	}
	iteration <- 0 
	criteria <- 99999
	while(criteria > eps){
		iteration <- iteration + 1
		prev_sol <- sleep$Sleep[indsleep]
		##E-step
		lm1 <- method(Sleep ~ log(BodyWgt) + log(BrainWgt) + NonD + Danger, data = sleep)
		##M-step
		sleep$Sleep[indsleep] <- predict(lm1)[indsleep]
		criteria <- sqrt(sum((prev_sol - sleep$Sleep[indsleep])^2))
	}
	res <- list("imputed" = sleep,
	"iteration" = iteration,
	lastmodel = lm1)
	return(res)
}

#Again we load the data set sleep, impute it and look at the imputed values in the variable Sleep:
data("sleep")
sleepImp <- EMregSleep()
missVals <- sleepImp$imputed$Sleep[ind$Sleep]
missVals
sleepImp$iteration

#However, we imputed with expected values, which will lead to a decrease of variance since we didn't account for the uncertainty and distribution of missing values (compare the variance estimation with missing values in chapter 8). To consider the variance, we sampled residuals (compare the bootstrapping residuals regression approach in chapter 8).
missVals + sample(residuals(sleepImp$lastmodel),length(missVals))

#Note that for a proper estimation of variances we may impute not only once but several times, resulting in several imputed data sets from where proper variances can be calculated (the multiple imputation approach). Or alternatively, we use the Bootstrap approach to consider the uncertainty of missing values.

#Previously wesaw that we needed 11 iterations. In addition, the OLS regression model might also be influenced from outliers, thus it is better to replace it with robust method. 

#We already see good results after the first iteration. We get slightly different results that are more trustable than using non-robust methods. 

#The OLS results may become corrupted, especially with outliers also in the predictors. However, we see that even with the worst initialization (also huge outliers in the predictors) the results looks fine (although we prefer the robust method anyhow). 

data("sleep")
##OLS regression 
lm_ols <- EMregSleep(method = lm, init = "worst")
##M-estimation 
lm_rlm <- EMregSleep(method = rlm, init = "worst", eps = 0.01)
lm_ols$imputed$Sleep[ind[, "Sleep"]]

#From the figures we can see that the OLS results are highly influenced by outliers. Compared to the previous estimates (using the bad initialization, not the worst one), the imputed values are too high. This is not as extreme when using an M-estimator, but compared to the implementation in the function irmi we underestimate the second and fourth value. 

#We have discussed how to impute one variable, but in general we want to impute all variables in a data set. The data set may also consist not only of continuous variables but also of a mix of continuous, semi-continuous, categorical, binary, and count variables. The robust EM-based imputation accounts for this and is implemented in the function irmi in the R package VIM.

data("sleep")
sleepImp <- irmi(sleep)
sleepImp[ind[, "Sleep"], "Sleep"]

#We see this is very clase to the initial solution where we took a better initialization of the missing values. This is an indication of the successfulness of irmi. We may use another method, such as mice (as irmi is usually used for multiple imputation):
library(lattice)
library("mice")
data("sleep")
em_mice <- mice(sleep, m = 1)
em_mice$imp$Sleep
##now with bad initialization in predictors 
sleep[is.na(sleep)] <- 2240
sleep$Sleep[ind[, "Sleep"]] <- NA
em_mice <- mice(sleep, m = 1)
em_mice$imp$Sleep

#We see that we get a completely different result, as soon outliers are present in the data set. This is also true for other implementations of the EM algorithm, that are only suitable when the data set is approximately multivariate normal. As soon as this is violated, the irmi might be a good choice. 
args(irmi)# Cool I can't believe that I forgot that one can look at the arguments within any function through the args() command. 
