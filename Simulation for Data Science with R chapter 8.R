### chapter 8 Applications of Resampling Methods and Monte Carlo tests:
##The bootstrap in regression analysis:
#We saw already in Chapter 7, Resampling Methods for estimation of the variance of MCD-based standard errors of correlation coefficients, resampling methods might be the only choice for estimating the variance for complex estimators. This is also true for regression analysis as soon as the classical, ordinary least squares (OLS) regression is - for good reasons - skipped, and more robust methods are chosen.

##Motivation ot use the boostrap:
#One might ask: "Why do we need to bootstrap to esitmate the variance of regression coefficients when analytical expressions are known for it?" The answer is simple, because only for the ordinary least-squares regression, in addition to many model assumptions, are the analytical expressions valid.

#Let's first look at the choice of more complex regression methods on a simple example using artificial data that best shows the problem that frequently occurs in practice.

library("robustbase")
data("hbk")
??hbk# interesting so this is a well known artificially generated data set that shows the "masking effect" using normal regression analysis. I will love to see how this problem effects classical regression analysis and how resampling can fix this problem.

##structure of the data 
str(hbk)#The documentation was correct this data set is indeed four dimensional.

#We next fit an OLS repression using Y as a response and all other variables as the predictors.
lm_ols <- lm(Y ~ ., data = hbk)
##Print the values in the model through of course the summary() function:
summary(lm_ols)

#From this output everything looks fine. For X2 and X3 we cannot reject the null hyposthesis that the regression coefficient is 0, the R2 is relatively high and the whole model is significant. 
summary(lm_ols)$coef

#We may also look at one residual diagnostic plot.
plot(lm_ols, which = 3)# A very useful description of this plot. Residual diagnostic plot. Fitted values against the square root of absolute standardized residuals.

#All standardized residuals are small and thus no outliers are detected in the following figure. For large fitted values, the absolute residuals are slightly larger than for the rest. All in all, the model looks acceptable. But is this really true? Might the model already be influenced from outliers so that the model outcomes are already so disturbed that we cannot detect abnormalities?

#Let's fit a robust MM-based regression and compare it to the previous OLS-based result:
lm_rob <- lmrob(Y ~ ., data = hbk)
summary(lm_rob)
#We see that the R2 is almost zero and that we must reject each variable in the model. In addition, the residuals in the diagnostic plot look different from the regular regression model summary. 
plot(lm_rob, which = 5)# Interesting this residual plot illustrates that there are a total of 10 outlier observations within the dataset.
dim(hbk)# this is particularly problematic since the data set that we're using only has 75 total observations. 

#we can clearly see from the following graphic that there are a few outliers present, and from the preceding summary of lm_rob we see that there is no dependency between x and y. 

#If we look at the pairwise scatterplot, we see that the robust regression method is correct and the OLS method completely fails. See the following faceted graphic.

pairs(hbk, pch = 20, cex = 0.6)
#From the preceding graphic we can observe that there is zero correlation between the variables and we also see the existence of few outliers. These outliers influence the OLS model in such a dramatic manner that it becomes completely corrupted, and the R2 will become high and two regression coefficients become highly significant. 

#With the classical OLS regression method, standard errors of the regression coefficients can be easily estimated using well known analytical expressions. However, for the much more reliable robust regression method, such an analytical expression is hardly known. Bootstrap is again, the method of coice for estimating the corresponding standard errors. Its application is as simple as for the OLS method, that is to say, the complesity of the application of the bootstrap is independent of the complesity of an estimator. 

##the most popular but often worst method:
#There are a total of two known methods in the estimation of the standard error of regression coefficients using the bootstrap.

#The first we show a method that is used in probably 95 percent of cases, but which in general overestimates confidence intervals. Afterwards we show a better method in case the model has good predictive power. This first method draws bootstrap samples from the whole dataset. 

#For Z, R bootstrap samples are drawn and for each bootstrap sample the model is estimated, resulting in R estimations (bootstrap replications) of the regression coefficients. From these bootstrap distributions, the confidnece intervals can then be calculated with, for example, the percentile method from Efron (which can be seen in chapter 7)

#For this following example the author uses again the prestige data set in the car package (I believe)
library(car)

#Rough draft of the regression equation: prestige ~ log(income) + women + type

#This means that we want to predict the variable prestige with the log values of income, percentage of incumbents who are wome, and type of occupation (blue, white, or professional). 

#The model can be fitted using robust methods. Since for MM-regression , the bootstrap is alread implemented in a very good way, we choose another robust regression method: least trimmed squares regression. In the output of the summary shown as follows, we see that the standard error estimated by the implementation of the function ltsReg is approximately 2.36. We remember this value for later use. 
rob <- ltsReg(prestige ~ log(income) + women + type, data = Prestige)
summary(rob)

#If we want to use the function boot from the R package boot, we should carefully prepare a boot function that the boot package can handle:
boot.lts <- function(x, indices){
	x <- x[indices, ]
	model <- ltsReg(prestige ~ log(income) + women + type, data = x)
	coefficients(model)
}

#Having this function defined, we can put everything to the boot function:
library(boot)
set.seed(123)
rob_boot <- boot(Prestige, boot.lts, 1000)
rob_boot

#Let's look at the coefficients for log(income) in more detail. We see that the standard error is approximately 3.944 while estimated from funciton ltsReg it was 2.36.

#It is easy to see that the distribution of the bootstrap replicates is bimodal and deviations show up, especially in the upper tail of the distribution. (This is most likely because the outlier values are being replicated 1000 times as well thus causing the regression model to become more skewed).

hist(rob_boot$t[,2], 50, xlab = "bootstrap repl., log(income)", main = "")# Interesting now I understand the following graphic is a representation of the predictor variable and the log(income) variable. 
rob_boot$t[,2]#the income variable is in the second column of the regression matrix. 
hist(rob_boot$t[,3], 50, xlab = "bootstrap repl,, women variable", main = "")#There really is a noticeable skew in the data of these two variables. 

#We can do some further computations, like to estimate the confidence intervals of the estimated regression coefficients. With function boot.ci one can estimate different types of confidence intervals, such as the bias corrected confidence intervals, confidence intervals with Efron's percentile method and confidence intervals by means of the BC_alpha method. For th coefficient log(income) the results are the following:
boot.ci(rob_boot, index = 2, type = c("norm","perc","bca"))
# Really cool tool to use. I will need to remember this function as well as read the documentation of its functionality.

#In chapter 7, Resampling Methods we learned about the jackknife after bootstrap plot to see the effect of single observations on the quantiles of the bootstrap distribution. This plot is shown for the coefficient log(income) and women on the following graphic. 
par(mfrow = c(2,1), mar = c(4,4,2,0.5))
jack.after.boot(rob_boot, index = 2, main = "log (income) coefficient") 
jack.after.boot(rob_boot, index = 3, main = "women coefficient")
#It's important to keep in mind that the index argument is used to designate which column you're willing to use for the jack.after.boot() plot graphic. 

#From these graphics we can observe that observations 58 and 54 have a strong influence on the bootstrap distribution; the boostrap distribution gets larger by excluding these observations. Observations 40, for example, also has high impact on the bootstrap distribution but decreases the distribution when kept out. Thus observation 2 can be seen as an outlier. 
#I really didn't get any of these observations when I looked at the graphic. I really need to learn about proper graphic visualization and analysis. 

##Bootstrapping by draws from residuals:
#In classical regression, one thinks of the X part as fixed (and independent). This can be considered by drawing from residuals instead of drawing bootstrap samples from Z. this approach is called the residual bootstrap.

#To obtain a bootstrap sample, a random error based on the bootstrapped residuals are added to the predicted value.

set.seed(123)
df <- data.frame(x = 1:7, y = 1:7 + rnorm(7))
#In the following code, we estimate the regression line and plot it to the data points. Afterwards we draw one bootstrap sample of residuals and add these residuals to the fitted values, that is; we get new values y_hat_1 = y_hat + standard_error_hat that we use as input to the regression problem to obtain a new regression line:
par(mfrow = c(1,2), mar = c(4,4,1,0.3))
##fit to original data 
lm_orig <- lm(y ~ x, data = df)
##plot the original data
plot(y ~ x, data = df)# Interesting, I didn't know that you could plot formulas with the plot() function. will need to remember this functionality for future excerises.
##Add regression line from original data 
abline(lm_orig)
##show the connection lines between original and fitted y 
segments(x0 = df$x, x1 = df$x, y0 = df$y, y1 = lm_orig$fit)#Cool I'll need to remember this line of code. I'm sure that Tilman Davies in the Book of R told me about this method but saddly I don't quite remember. 

##fitted y 
points(df$x, lm_orig$fit, pch = 20, col = "red")
legend("topleft", legend = c("y", expression(hat(y))), pch = c(1, 20), col = c(1,2))

##second plot 
##plot of fitted values 
plot(lm_orig$fit ~ df$x, col = "red", pch = 20, ylab = "y", xlab = "x")
##bootstrap sample by adding sampled residuals
y1 <- lm_orig$fit + sample(lm_orig$res, replace = TRUE)
##new bootstrap sample
points(df$x, y1, col = "blue", pch = 3)# so cool. I'll need to experiment with this method and the other methods through the book. This is pretty neat.
##connection lines new bootstrap sample to fitted values from original data
segments(x0 = df$x, x1 = df$x, y0 = lm_orig$fit, y1 = y1, col = "blue")
##regression line from bootstrap sample 
abline(lm(y1 ~ df$x), col = "blue", lty = 2)
abline(lm_orig, col = "black", lty = 1)
#Interesting. Why did the residual regression line deviate so much from the original regression line? This is pretty weird. 

#this approach cannot be repeated, that is to say; new bootstrap samples would lead to new regression lines.

#Let's go back to a real case. We take again the Prestige data from the R car package and estimate the standard error of coefficients from the robust regression model via least trimmed squares regression. Let's first fit the model on original data.

Prestige <- na.omit(Prestige)
##fit model on original data 
rob2 <- ltsReg(prestige ~ log(income) + women + type, data = Prestige)
#We extract residuals and fitted values for later use, and also we need the model matrix later on. Note that this model matrix is fixed and thus only estimated from the original sample. 
residuals <- residuals(rob2)
fit <- fitted(rob2)
##Fix X, model matrix 
X <- model.matrix(rob2, Prestige)[,-1]

ltsBoot2 <- function(x, indices){
	y <- fit + residuals[indices]
	model <- ltsReg(y ~ X)
	coefficients(model)
}

#Finally, we can do the residual based bootstrap:
rob2_boot <- boot(Prestige, ltsBoot2, R = 2000)
rob2_boot
rob_boot
#We can see that the standard error is smaller compared to the previous approach (sampling from Z). Also the distribution of the bootstrap replicates related log(income) and women is now approximately normal and looks better shaped than with the previous approach. 

#the author is right the standard errors are cut down by more than half in the case of some variables. This is a pretty awesome technique.

par(mfrow = c(1,2), mar = c(4,4,1,0.2))
hist(rob2_boot$t[,2], 50, xlab = "bootstrap repl., log(income)", main = "")
hist(rob2_boot$t[,3], 50, xlab = "bootstrap repl., women", main = "")
#this method worked out perfectly the distributions look more normal than the other method (using Z values).

#The jackknife after bootstrap plot confirms the comparatively better properties of the residual-based bootstrap than with the bootstrapping from Z before:
par(mfrow = c(2,1), mar = c(4,4,2,0.5))
jack.after.boot(rob2_boot, index = 2, main = "log(income) coefficient")
jack.after.boot(rob2_boot, index = 3, main = "Women coefficient")

#(additional note) In case of using the residual bootstrap in combination with robust methods, the bootstrap can be improved by sampling n residuals out of n - m outlier free residuals (m... amount of outliers detected).

##Proper variance estimation with missing values 
#Very often in practice, missing values are a major problem. Standard routines for estimation are typically not designed to deal with missing values. In the following we discuss a method to adequately deal with missing values when estimating the variance/uncertainty of an estimator.

#Often one will omit those observations that include missing values from the data set. However, this decreases the sample size and thus increases the variance of estimators, and in addition this may cause biased estimates if missing values are missing at random, that is; if the probability of missingness depends on covariates.

#To work around this problem, another, better solution is to impute missing values. For some applications the imputations are done in a way to minimize a prediction error. for other applications it is important that the variance of an estimator is estimated in a proper manner. If one imputes values to minimize a prediction error, the variance of an estimator would be underestimated. But also if missing values are imputed by a ingle imputation method, the variance is underestimated since fixed values are used as imputations, rejecting the variability of the imputation. Thus for proper impuation, multiple imputation methods are usually applied. this means that not just one imputation is made using a probabilistic impuation method, but several. This results in multiple files, proper variances of estimators can be obtained by using so called combining rules of each of the estimated variances.

##Example of imputation using the bootstrap and jackknife method (originally written by Little and Rubin 2002).

#Let X be a sample of p variables and n independent observations including missing values. The estimation of confidence intervals regarding a point estimate population parameter_hat from a population parameter is as follows. Note that the trick is to impute the bootstrap samples, not the original data. The philosophy again, is that the original sample is the best information you have, and the bootstrap samples of this original sample mimic the population, also regarding the missing values.

#for r = 1, ...., R, repeat:
	#Draw a bootstrap sample X_i* of the original (non-imputed) sample X.
	#Impute the missing values in X_i*.
	#Estimate the parameter population_parameter*_i = f(X*_i).
	
#This results in R bootstrap replicates for population_parameter*_i, i = 1, ...., R. this bootstrap replicate distribution is then used for determining the confidence intervals or standard errors with respect to population_parameter_hat, for example, by using the percentile method from Efron.

#Let's do this in practice. In the following example, confidence intervals are estimated for data with missing values using the mentioned bootstrap. To keep it simple, population_parameter should be the arithmetic mean of one variable. Repeatably, bootstrap samples are drawn from the sample, the missing values in the bootstrap samples are imputed, and finally the arithmetic mean is estimated from the bootstrap samples. We use the sleep data set that includes missing values.
library(VIM)
data("sleep")
#Let's first analyze the missing values structure a bit. The left plot for the following graphic shows the number of missing value in each variable of the sleep dataset.
aggr(sleep, numbers = TRUE, prop = FALSE, cex.axis = 0.75)#this data set looks rather small. I need to look at the dimensions and the amount of na values the dataset carries.
dim(sleep)# No wonder 62 observations and 10 variables 

#For example, 42 observations are fully observed, 9 observations have missing values in both the third and fourth variable, and so on. 

#The so-called matrixplot() function shows the whole data matrix by variable BrainWgt. The darker the lines the higher the corresponding values of the data matrix. Red lines correspond to missing values in the data matrix. From the following graphic we see that the probability of missings in NonD, Dream, and sleep might increase with higher values on BrainWgt:
par(mar = c(6,4,0.2,0.2))
matrixplot(sleep, sortby = "BrainWgt", interactive = FALSE)

#A bootstrap sample (must) show similar behavior since we resample with replacement from the sample. From the following graphic we can observe that the number of missing values changed slightly. The frequencies according to the missing patterns differ. Instead of 42 observations without missing values, only 38 observations do not include any missings in the bootstrap sample. Also the frequencies of, for example, missing in NonD and Dream is now 11 instead of 9 before.
dev.new()
dev.set(3)
set.seed(123)
sleep_boot1 <- sleep[sample(1:nrow(sleep), replace = TRUE),]
aggr(sleep_boot1, numbers = TRUE, prop = FALSE, cex.axis = 0.75)
dev.set(1)
aggr(sleep, numbers = TRUE, prop = FALSE, cex.axis = 0.75)

#In the matrixplot, it is visible that slightly more patterns are selected for NonD and Dream combinations of missings. Also in variable Gest, the probability of missingness is higher for small values of BrainWgt as before.
par(mar = c(6,4,0.2,0.2))
matrixplot(sleep_boot1, sortby = "BrainWgt")

#This simulation lacks a bit since we assume fixed values for missings. However, in practice a missing value has its own distribution. A way out would be to look at convergence rates for confidence intervals. However, we want to keep it simple and want to show how this bootstrap approach can be applied.

#We give results for single imputation:
n <- nrow(sleep)
imputed <- kNN(sleep, imp_var = FALSE)
##Time difference of 0.0347321 secs 
ci_single <- quantile(replicate(10000, mean(imputed[sample(1:n, replace = TRUE), "Sleep"])), c(0.025, 0.975))
ci_single

#Finally, we estimate the confidence interval with our proposed bootstrap approach. Here, the bootstrap samples are taken from non-imputed data. The following line of code needs longer computational time.

ci_boot <- quantile(replicate(10000, mean(kNN(sleep[sample(1:n, replace = TRUE),], imp_var = FALSE)$Sleep)), c(0.025, 0.975))
#Note to self don't use this function since my macbook air is unable to keep up with the computational expense. 

ci_boot <- quantile(replicate(100, mean(kNN(sleep[sample(1:n, replace = TRUE),], imp_var = FALSE)$Sleep)), c(0.025, 0.975))
#I was forced to change the times argument in the replicate function to 100 from 10000 since the command was taking too long to carry out. This oversight might cause my values to differ from the book and also compromise the statistical accuracy of this imputation method. 
#The solution is 9.085 (for the 2.5% mark) and 11.65 (for the 97.5% mark).

#we see that this leads to a slightly larger confidence interval since we considered the uncertainty of imputation.
#Of course we can also use the jackknife method for the estimation of confidence intervals when the data includes missing values. The approach is very similar to the bootstrap approach.

#For j = 1, ..., n:
	#Impute the jackknife sample X_(i).
	#Estimate the parameter population parameter of the imputed dataset.
	#From the resulting jackknife replicates distribution, estimate the confidence intervals using the jackknife method. 
	
##Bootstrapping in time series:
#Two methods are often used in bootstrapping of time series:
	#To estimate a model and draw from the residuals (see second last section on bootstrapping regression models by bootstrapping residuals).
	#Moving blocks bootstrap methods.
	
#We concentrate in the following, on the moving blacks bootstrap. It is a method that is often applied and mentioned in literature, but with limited success. To show the limitations of this approach is one goal of this section.

#The idea is to divide the data in blocks and to sample with replacement within blocks. This allows us to not completely ignore the relationship between the observations. Relationships between observations are typically present in time series. For example, the next value will depend on the previous value. Think also on the trend, seasonality, and periodicity. (This sounds very much like the K-fold cross validation. Will need to see if this is the case, or rather if both share the same methodology).
#In principle, the time series can be divided in non-overlapping or overlapping blocks. 
#We will show an overlapping moving blocks bootstrap for estimating the autocorrection. First we generate some toy data:
set.seed(123)
tseries <- rnorm(50)
## introduce autho-correlation 
tseries[-1] <- tseries[-1] + tseries[-50]
#the time series looks as illustrated by the following graphic:
plot(ts(tseries), ylab  = "values")# Interesting, will need to look into the ts() function. The ts might mean time series. 

#Our moving blocks bootstrap needs the data, the blocksize, and the number of replications as input:
mbb <- function(x, R = 1000, blocksize = 6){
	##initialization 
	nc <- length(x)
	lagCorMBB <- ct <- numeric(R)
	seriesB1 <- numeric(nc)
	##function for moving blocks bootstrap
	corT <- function(x = tseries, N = nc, b1 = blocksize){
		##for N/b1 blocks
		for(i in 1:ceiling(N/b1)){
			##endpoint of block
			endpoint  <- sample(b1:N, size = 1)
			##put blocks together, bootstrap sample 
			seriesB1[(i - 1)*b1 + 1:b1] <- x[endpoint - (b1:1)+1]
		}
		seriesB1 <- seriesB1[1:N]
		##autocorrelation 
		a <- cor(seriesB1[-1], seriesB1[-N])
		return(a)
	}
	ct <- replicate(R, corT(x))
	return(ct)
}
# I really can't believe that you can actually put a function within a function. In addition I have no clue what this function does or rather how to even begin understanding. Will need to learn more about the basics of R programming and basic time series equations. 

#Now let's apply this function on our time series:
mb <- mbb(x = tseries, R = 10000)
##first 10 bootstrap replicates:
mb[1:10]
##autocorrelation coefficient from mean of bootstrap replicates
mean(mb)

#It is natural that if the time series is not white noise then the arithmetic mena of moving block bootstrap replicates is smaller than the point estimate from the data itself, since we lose some autocorrelation by partitioning the time series into new blocks. We see the autocorrelation (of lag 1) of the data in comparison. 
acf(tseries)$acf[2]

#However, we are in any case only interesting in estimating the 95% confidence interval (percentile method) of the autocorrelation coefficient:
qu_mbb <- quantile(mb, c(0.025, 0.975))
qu_mbb

#In comparison, the default method in the forecast package for the confidence interval of the autocorrelation coefficient is reported as the following:
library(forecast)
ac <- taperedacf(tseries)
ac$upper[1]
ac$lower[1]# interesting the author says that this value should be -0.0134 instead of 0.0134. Will need to find out what the problem is. 
#In addition this confidence interval method is more classic than the corresponding moving blocks bootstrap method.

#We saw how to implement a moving blocks bootstrap but we didn't mention in what situation a moving blocks bootstrap might not work properly. Cutting blockss and regjoining them will automatically destroy the trend and would lead to non-reliable results whenever the time series is not stationary. Thus before the application of a moving blocks bootstrap, the time series must be de-trended. In addition, the moving blocks bootstrap may only work for simple time series models, such as for the autoregressive of lag 1 (AR 1) process. Here it is assumed that the next value depends only on the previous one. For more complicated approaches such as general ARIMA process, the moving blocks bootstrap may not give valid inference statistics. 

##Bootstrapping in the case of complex sampling designs:
#We already saw many applications where all these samples were drawn completely at random. However, this is often not the case when one has little information on a finite population, or when the data collected is based on a complex survey design. Of course such information is used to draw a sample in such a manner that costs are minimal. In other words, as an example from business statistics: a lot of small and median sized companies exist in Austria but not many large ones. For precise estimates we need all the largest companies (selection probability 1), but the probability of selection of small companies can be much lower. A complex survey design allows us to draw a good sample with minimal costs. 

#In complex survey sampling, individuals are therefore sampled with known inclusion probabilities pi_i (i = 1, ..., N) from a population size N to end up with a sample size of n. The inclusion probabilities can differ between strata (partitions of the population) or can even be different for each individual in the sampling frame. 

#Usually the population is divided into K strata with n_k(k = 1, ..., K) observations drawn from stratum k. Then, bootstrap samples can be constructed by resampling n_k observations with replacement from the k-th stratum. Likewise, if observations are drawn into the sample in clusters rather than individually, for example; when drawing household and collect information on each household member, then the bootstrap should resample clusters rather than individuals. This is often mentioned as the naive bootstrap.

#Of course, the bootstrap can be applied to complex sampling designs (involving, for example, stratification, clusting and case-weighting) by resampling from the sample data in the same manner as the original sample was selected from the population. However, as mentioned before, the calibration should be taken into account. 

#Let's show the problem on a very simple (and simplified) toy data example. Consider the following simple sample data set and let's ignore that, typically, such data has a household structure.
x <- data.frame("location" = rep("Asten", 8), "income" = c(2000, 2500, 2000, 2500, 1000, 1500, 2000, 2500), "weight" = c(1000, 500, 1000, 500, 1500, 1000, 1000, 2000))
x
#We see a sample of eight people in Asten reporting the income and the sampling weights. We assume that the sampling weights are already calibrated and that, for example, the first observation stands for 1000 observations in the population, the second observation stands for 500 observations in the population, and so on. Thus, all in all, we can assume that the population size of Asten is 8500 people.
sum(x$weight)
#The weighted total (Horwitz Thompson estimation), or the estimation total income of all people in Asten is:
sum(x$income * x$weight)
#Let's draw one bootstrap sample:
set.seed(123)
y <- x[sample(1:8, replace = TRUE),] #Bootstrap sample
y
#The estimated total income of the people living in the village of Asten is then:
#non-calibrated estimation
sum(y$income * y$weight)
#this value is quite high. The condition N = 8500 is violated and thus the estimation of total income is distorted. For this boostrap sample, the population size of Asten is:
sum(y$weight)#the resulting value is 11000 (which is larger than the survey's estimation of 8500)
#This is surely not equal to 8500. This explains why the estimated income is completely overestimated from the first bootstrap.

#Since we know that 8500 people live in Asten and surely not 11000, we will use this information about the population and we will calibrate the bootstrap sample according to the population information. In this case this is easy, we just have to multiply each weight by a constant 8500/11000:
constant <- sum(x$weight) / sum(y$weight)
##calibrate estimation
sum(y$x*y$w*constant) 

#This example demonstrated the simplest case. Usually the sample must be calibrated according to various known population characteristics. However, this example clearly shows the need of a calibrated bootstrap, formally show in the following. 

#Remember, the naive bootstrap algorithm for the estimation of the standard deviation: To see the steps look at page 276. 

#The calibrated bootstrap needs an additional step between point 1 and 2 of the naive bootstrap.
#First, calibrate the sampling weights so that the bootstrap sample exactly fits to known population characteristics, where the sampling weights should be changed as less as possible, that is; the sampling weights are multiplied by so-called g-weights that should be as clase to 1 (optimization problem). These g-weights are typically estimated by ranking methods such as iterative proportional fitting or least squares regression methods. 

#We apply this calibrated bootstrap on a more complex data set in the following example which is about how to estimate the at-risk of poverty rate from the European Union statistics on Income and Living Conditions Survey. 

#The at risk of poverty rate is denfined as the ratio of people with equalized household income below the poverty threshold. The poverty threshold corresponds to 60 percent of the median income of the population. 

#ARPR = P(X_U < 0.6 * med(X_U)) = F(0.6) with X_U being the equalized household income of the population. F_U is the distribution function of income. 

#For estimating the poverty rate, the sampling weights have to be considered. First we have to estimate the poverty threshold by:
	#To see the following equation information look at page 276.
	
#These estimations can be practically done with the r package, laeken. The calibrated bootstrap as available with the function variance. Note that the totals are usually determined by a population. here we estimate it from the sample:
library(boot)
library(MASS)
library(laeken) 
data(eusilc)
##point estimate of poverty rate 
a <- arpr("eqIncome", weights = "rb050", data = eusilc)
##bootstrap with calibration 
##define auxiliary 0-1 variables for regions 
aux <- sapply(levels(eusilc$db040), function(l, x) as.numeric(x == l), x = eusilc$db040)
##retrieve population totals from underlying sample 
totals <- sapply(levels(eusilc$db040), function(l,x,w) sum(w[x==l]), x = eusilc$db040, w = eusilc$rb050)
#bootstrap variance 
variance("eqIncome", weights = "rb050", design = "db040", data = eusilc, indicator = a, X = aux, totals = totals, seed = 123)# There we go. It took me a while to recreate this line of code. The addition that might need further scrutiny is which out for the way l and 1 is printed in the text of the book. It's very curious though since I could have sworn that the l was indeed a 1 in the function() arguments. Very interesting, I might need to check through this code line by line to understand that rational behind it. 

#We see that the point estimate of the at-risk-at-poverty rate is 14.444 and the confidence interval estimated by the calibrated bootstrap is [13.878; 15.193]. Note that in R there is no formula available for estimating the variance of the at-risk-at poverty rate with an analytical expression. This is also true for many other poverty estimates. Existing formulas in the literature are complex and depend on assumptions and vary between sampling designs. The calibrated bootstrap is the only chance to estimate the variance in a user-friendly manner. 

##Monte Carlo tests:
#Do you know the test statistics of the multivariate Anderson-Darling test for multivariate normality? Don't worry, these test statistics have only been approximated for a few significance levels in terms of simulation experiments, and it is generall unknown. But then how we can estimate the value of the test statistics for a given number of observations, number of variables and a certain significance level? The answer is easy, and the procedure is as easy as for much simpler tests. We can do it with resampling methods for testing - the monte carlo tests.

##A motivating example:
#Before we got to more formal descritpions of such tests, we introduce the Monte Carlo resampling test with a long introductory example. This should show why a Monte Carlo test works. The following example using body temperature data is motivated by a lecture given by Friedrich Leisch.

#We first took some data, body temperature and heart rate from 65 men and 65 women. 
temp <- read.table("http://venus.unive.it/romanaz/statistics/data/bodytemp.txt", header = TRUE)
temp$gen <- factor(temp$gen, labels = c("male","female"))
temp
str(temp)#so according to this description there are 130 total observations and 3 variables within the data set. 

#Let's visualize first, the densities related to male and female temperatures in the following graphic.
library(ggplot2)
ggplot(temp, aes(x = temp$tf, color = gen, linetype = gen)) + geom_density(size = 1.2) + theme(text = element_text(size = 16)) + theme_bw()

#From this ggplot representation we see the different distributions according to male and female. Females have on average, higher temperatures in this dataset. We are coming back to the question on whether the Null hyposthesis of equal population means can be rejected. 

#First issue is to test if the dataset is approximately normal, also an important assumption in many other tests. More precisely we want to know if the Null hyporthesis sample that is drawn from a normal distribution can be rejected. We save the temperatures in its own vector, just for better visibility of code afterwards:
temperature <- temp$tf

#First let us visualize the empirical distribution function that gives mass 1/n at each sorted observation.

n <- length(temperature)
temperature <- sort(temperature)
y <- (0:(n - 1))/n
#We can now plot the empirical distribution function, the sorted temperature values against a vector of length n of equidistant values between 0 and 1. The result is visible in the following graphic.
plot(temperature, y, pch = 20, cex = 0.3)
lines(temperature, y, type = "S")

#We now can ask if these empirical values are drawn from a normal distribution. One possibility is to use diagnostic tools such as QQ plot, but this time we want to make a decision with numerical tools. Let's calculate the theoretical normal distribution with mena and standard deviation from our empirical data, plot and add it to the figure.
plot(temperature, y, type = "S")
m <- mean(temperature)
s <- sd(temperature)
yn <- pnorm(temperature, mean = m, sd = s)
lines(temperature, yn, col = 2)

#This is not too bad, but how do we get confidence bands for this red line?
#We can simulate the confidence bands through drawing random samples according to the null hypothesis using parameters estimated from the empirical sample data. In this case we draw from a normal distribution with mean and standard deviation from your temperature data, and see how much the resulting empirical cumulative distribution functions fluctuate. One draw is done as follows.
z <- round(sort(rnorm(n, mean = m, sd = s)), 1)
#We can repeat this and draw these lines in the plot:
set.seed(123)
plot(temperature, y, type = "S")
for(k in 1:100){
	z <- rnorm(n, mean = m, sd = s)
	lines(sort(z), y, type = "S", col = "green")
}
# Interesting idea, in place of creating a polygon() the author simulated random data using the dataset's mean and standard deviation values as benchmarks. I believe that Tilman davies almost used the same method except that he only generated random numbers because he wanted to create a polygon showing the confidence intervals along the entirity of the model. 

#The empirical cumulative distribution function of our original data should now have disappeared, that is; the black stepwise line representing the empirical cumulative distribution function of our temperature data should not be visiable anymore. Thus the hypothesis of normality isn't complete nonsense.

#Let's save the results from 1000 simulations from a normal distribtuion in a dataset:
Z <- NULL
for(k in 1:1000){
	z <- rnorm(n, mean = m, sd = s)
	Z <- cbind(Z, sort(z))
}
dim(Z)

#Z contains in each column, a sorted sample of size 130 from a normal distribution with mean m and standard deviation s. Another view is that z contains in each row, 1000 estimates for the n/130-quantile of a normal distribution with mean m and standard deviation s.
##mean of original temperature data 
m
#simulated mean
(mean(Z[65,]) + mean(Z[66,]))/2# Now I understand the values are in Fehrenheit in my console and because of that our values will most likely differ. 
#simulated median:
(median(Z[65,]) + median(Z[66,]))/2

#The next plot shows an estimate of the normal distribution based on our random draws from a normal distribution. We also add lower and upper bounds - the confidence interval with significance level 0.05.
plot(temperature, y, type = "S")
middle <- apply(Z, 1, median)
lines(middle, y, col = "blue", lwd = 2, type = "S")
##lower and upper bound 
lower <- apply(Z, 1, quantile, prob = 0.025)
upper <- apply(Z, 1, quantile, prob = 0.975)
lines(lower, y, col = 2, lwd = 2, type = "S")
lines(upper, y, col = 2, lwd = 2, type ="S")

#Our empirical data represented by its cumulative distribution function (the black stepwise line) is almost always inside the confidence bands. Again this is an indication that the normality assumption might not be rejected. 

#Probably the most popular test for distributions is the Kolmogorov-Smirnov test. Its test statics is given by the maximum deviation of the observed empirical cumulative distribution function from a theoretical one. 

#Let's look at these deviations for each given temperature, reported in the bottom of the following graphic. We also mark the maximum deviation with a big circle:
par(mfrow = c(2,1), mar = rep(1,4))
plot(temperature, y, type = "S")
lines(temperature, yn, col = 2)
lines(lower, y, col = 2, lwd = 2, type = "S")
lines(upper, y, col = 2, lwd = 2, type = "S")
plot(temperature, y - yn, type = "h")
abline(h = 0)
##Maximum deviation 
D <- max(abs(y-yn))
w <- which.max(abs(y-yn))
points(temperature[w], y[w] - yn[w], col = 2, pch = 16, cex = 3)
#The lower most graphic is the Related differences between ECDF of original data and the theoretical ECDF.

#How likely is it that the empirical cumulative distribution function of normal distribution has at least the observed deviation?

#We already simulated 1000 samples. We take a look at their maximum deviation by again doing the same that we did with the temperatures before.
##theoretical distributions:
Z1 <- pnorm(Z, mean = m, sd = s)
##y will be recycled column wise 
##extract the maximum for each column 
D1 <- apply(abs(y - Z1), 2, max)
#If we look at the distribution of D1 and compare it to the maximum deviation from our original data (D), we see that the value of D is not that unusual. 
summary(D1)
summary(D)

#We can ask how many times our simulated maximum deviations are larger than the maximum deviation from our original data set. And even better, we may ask the ratio of it. It's already the p-value.
mean(D1>D)

#Let's have a look at the Kolmogorov smirnov test here:
ks.test(temperature, "pnorm", mean = m, sd = s)#Interesting the ratio above has the same vale as the ks.test() function for the p_value. 

#If we increase the number of columns of z, that is;the number of draws from a normal distribution, then the mean(D1>D) should be equal to the p-value of the Kolmogorov Smirnov test. (beside the problems of the Kolmogorov smirnov test with rounded values). 

##the permutation test as a special kind of MC test:
#In the following example, the arithmetic mean of 45 US americans (workers and employees) from 1950 are compared:
data(Duncan, package = "car")
x <- subset(Duncan, type %in% c("bc","wc"), select = c("income","type"))
x$type <- factor(x$type)
dim(Duncan)
head(x, 4)

#the classical t-test to test the previous mentioned null hypothesis is:
t.test(income ~ type, data = x)
#the p-value is 0.01669. thus the null hypothesis can be rejected for a significance level of 0.01 but not for a significance level of 0.05.

#A strict assumption of the classical t-tests for two independent samples is that the population is normally distributed and that the variances should be equal. In addition, we basically do not know the test statistics when we replace the arithmetic mean with a, for example, robust estimator.

#All these assumptions and weaknesses in translating the test for arithmetic means to other location estimates are not present in Monte Carlo tests.

#The general idea is to mimic the null hypothesis, that is, to simulate values from the null hypothesis, apply the same test statistics to the original data and to the simulated one and compare these test statistics.

#The p-value from the t-test shown before can also now be calculated by the permutation test (other Monte Carlo tests follow later).
	#Estimate the absolute difference of means of the two groups in the original sample (with original classes). Denote this as population parameter.
	#simulate from the null hypothesis by permuting the grouping/classes Ranom groups mimic the null hypothesis of equal means. Calculate the absolute difference between the means with the randomized grouping structure. Denote the results as population parameter*_1. Repeat this at least:
		#R = 1000 times population parameter*_r, (r = 1, ..., R)
	#The p-value is then given by #(population parameter*_r >= population parameter_hat)/R
	
#The permute the grouping structure, we simply use the function sample (without replacement):

##first 6 observations with permuted grouping structure.
head(cbind(x, "p1" = sample(x$type), "p2" = sample(x$type), "p3" = sample(x$type)))

#The groups still have n_1 and n_2 observations but their observations are randomized according to the group belongings. this mimics the null hypothesis of equal means. 

#Now let's write a permutation test in R. We use the class htest of R to receive standardized print output: 
##define test statistics (workhorse)
teststat <- function(vals, group, lev){
	g <- sample(group)
	abs(mean(vals[g == lev[1]]) - mean(vals[g == lev[2]]))
}

##permutation test:
permtest <- function(x, g, R = 1000, conf.level = 0.95){
	##levels of the group vector 
	lg <- levels(g)
	##test statistics for original groups 
	mdiff <- abs(mean(x[g == lg[1]]) - mean(x[g==lg[2]]))
	##test statistics for permuted group data 
	z <- replicate(R, teststat(x, g, lg))
	##make nice print output 
	DATA <- paste(deparse(substitute(x)), "by", deparse(substitute(g)))
	alpha <- 1 - conf.level
	conf.int <- quantile(z, prob = c(alpha/2, (1-alpha)/2))
	attr(conf.int, "conf.level") <- conf.level
	res <- list(statistic = c(mdiff = mdiff), 
				p.value = mean(abs(z) > abs(mdiff)),
				parameter = c(nrep = R),
				conf.int = conf.int,
				data.name = DATA,
				method = "Permutation test for difference in means")
	class(res) <- "htest"
	res
}

#Now we can apply the permutation test to the Duncan data on income and type:
permtest(x$income, x$type, R = 10000)
# I can't believe that this function actually worked much less resulted in such a nice looking output. Will need to reverse engineer this line of code to see how exactly this result was created. Very interesting none the less.

#Also the permutation test would reject for a significance level of 0.05, but also for 0.01. The permutation test can be applied to any test problem where the groups/ classes of a variable play a central role.

##A monte carlo test for multiple groups:
#If more than one group should be compared, typically ANOVA is the choice. However, this could also be done by a pairwise t-test.
data(Duncan, package = "car")
pairwise.t.test(Duncan$income, Duncan$type)

#What is meant by p-value adjustment? The probability to reject (to reject the null hypothesis when the null hyposthesis is true) all k tests is the product over all significance levels, that is; (1 - confidence level)^k. If the confidence level is 0.05 and the k = 100 then the probability of rejection is (1 - 0.05)^100 = 0.994. this is also true if, for example, the data drawn from a normal distribution, randomly chooses 100 groups and a normality test is made since every single test will reject with probability of 0.05 when the data is drawn from normal distribution and a normality test is made. Thus, one of the common problems of significance testing is the tendency to multiply comparisons to yield spurious significance differences even where the null hypothesis is true. Therefore, p-values must be adjusted for multiple comparisons.

#The Bonferroni correction mulitplies all p-values by the number of tests, the Holm correction multiplies the smallest p-value with n, the second with n - 1, and so on.

#With the pairwise.t.test we looked for differences between any pairwise combination of groups. Another possible question is whether the main value of all groups is the same:
mean(Duncan$income)
library("dplyr")
Duncan %>% group_by(type) %>% summarize(mean = mean(income))#According to this pipe generated representation. The mean values are not equal to one another.

#Our test statistic is the maximum absolute value of all test statistics, calculated by the following:
tstat <- function(x, mu = 0){
	(mean(x) - mu)/(sd(x) / sqrt(length(x)))
}

stats <- tapply(Duncan$income, Duncan$type, tstat, mu = mean(Duncan$income))
stat <- max(abs(stats))
stat
#Instead of thinking of a way to find the test statistics in an analytic manner, we are laxy and remember the Monte Carlo way of easy living. Note that for a Monte Carlo test it is only important to obtain values of test statistics by respecting the null hypothesis. We simulate random numbers from the null hypothesis, or if we know something about the distribution of the test statistics we can simulate random numbers directly from this distribution. In our case we know that the distribution of the test statistics of a z-test is a t-distribution.
maxt.test <- function(x, g, R = 10000, conf.level = 0.05){
	m <- mean(x)
	stat <- tapply(x, g, tstat, mu = m)
	stat <- max(abs(stat))
	gsize <- table(g)
	z <- NULL
	for(k in 1:length(gsize)){
		##From a t-distribution 
		z <- cbind(z, rt(n=n, df = gsize[k]-1))
	}
	##z now is a list with length(gsize) elements 
	##we need the maximum absolute value for each element
	z <- abs(z)
	z <- z[cbind(1:n, max.col(z))]
	##Make nice print output
	DATA <- paste(deparse(substitute(x)), "by", deparse(substitute(g)))
	alpha <- 1 - conf.level
	conf.int <- quantile(z, prob = c(alpha/2, (1-alpha)/2))
	attr(conf.int, "conf.level") <- conf.level
	res <- list(statistic = c(stat = stat),
					p.value = mean(z > stat),
					parameter = c(nrep = R),
					conf.int = conf.int,
					data.name = DATA,
					method = "Maximum t-test")
	class(res) <- "htest"
	res
	
}

#Now let us apply this test on the Duncan data set with three groups of types:
maxt.test(Duncan$income, Duncan$type, n =60)
maxt.test(Duncan$income, Duncan$type, n = 64.54)
# The p-value and the confidence intervals are all different by a little. I believe this is because of my patch for the code where there was an error within the rt() function assembly. My console couldn't find the n object to full in the n argument and, to be honesty, I was surprised that that author over looked this detail in his code. Will need to see what he was trying to get at through this function call. 

#I still can't find what he was intending to do with the n = n argument. Will need to look into this problem later on in my studies once I obtain enough experience in statistical programming and monte carlo methods. 

#We see that we can reject the null hypothesis.
#Another possibility is to make a permutation test.
maxp.test <- function(x, g, R = 10000, conf.level = 0.05){
	m <- mean(x)
	stat <- tapply(x, g, tstat, mu = m)
	stat <- max(abs(stat))
	z <- numeric(n)
	#great the n argument again. Will really need to find where the n object is. 
	for(k in 1:n){
		g1 <- sample(g)
		z[k] <- max(abs(tapply(z, g1, tstat, mu = m)))
	}
	retval <- list(tstat = stat, pval = mean(z > stat), name = "Permutation maximum t-test")
	retval
	##Make nice print output 
	DATA <- paste(deparse(substitute(x)), "by", deparse(substitute(g)))
	alpha <- 1 - conf.level
	conf.int <- quantile(z, prob = c(alpha/2. (1-alpha)/2))
	attr(conf.int, "conf.level") <- conf.level
	res <- list(statistic = c(stat = stat),
							p.value = mean(z > stat),
							parameter = c(nrep = R),
							conf.int = conf.int,
							data.name = DATA,
							method = "Permutation maximum test")
	class(res) <- "htest"
	res
}

#Again we apply the test on the Duncan data:
maxp.test(Duncan$income, Duncan$type)
#I guess I will have to put off these exercises for now since again I con't seem to find the n object to make these exercises actually work. Will need to come back to these exercises after I gain more experience with statistics. Currently not confident enough to mess around with the formulas and algorithms. 

#My best patch for these exercises:

maxt.test <- function(x, g, R = 10000, conf.level = 0.05, n){
	m <- mean(x)
	stat <- tapply(x, g, tstat, mu = m)
	stat <- max(abs(stat))
	gsize <- table(g)
	z <- NULL
	for(k in 1:length(gsize)){
		##From a t-distribution 
		z <- cbind(z, rt(n=n, df = gsize[k]-1))
	}
	##z now is a list with length(gsize) elements 
	##we need the maximum absolute value for each element
	z <- abs(z)
	z <- z[cbind(1:n, max.col(z))]
	##Make nice print output
	DATA <- paste(deparse(substitute(x)), "by", deparse(substitute(g)))
	alpha <- 1 - conf.level
	conf.int <- quantile(z, prob = c(alpha/2, (1-alpha)/2))
	attr(conf.int, "conf.level") <- conf.level
	res <- list(statistic = c(stat = stat),
					p.value = mean(z > stat),
					parameter = c(nrep = R),
					conf.int = conf.int,
					data.name = DATA,
					method = "Maximum t-test")
	class(res) <- "htest"
	res
	
}

maxt.test(Duncan$income, Duncan$type, n =length(Duncan))

maxp.test <- function(x, g, R = 10000, conf.level = 0.05, n){
	m <- mean(x)
	stat <- tapply(x, g, tstat, mu = m)
	stat <- max(abs(stat))
	z <- numeric(n)
	#great the n argument again. Will really need to find where the n object is. 
	for(k in 1:n){
		g1 <- sample(g)
		z[k] <- max(abs(tapply(z, g1, tstat, mu = m)))
	}
	retval <- list(tstat = stat, pval = mean(z > stat), name = "Permutation maximum t-test")
	retval
	##Make nice print output 
	DATA <- paste(deparse(substitute(x)), "by", deparse(substitute(g)))
	alpha <- 1 - conf.level
	conf.int <- quantile(z, prob = c(alpha/2. (1-alpha)/2))
	attr(conf.int, "conf.level") <- conf.level
	res <- list(statistic = c(stat = stat),
							p.value = mean(z > stat),
							parameter = c(nrep = R),
							conf.int = conf.int,
							data.name = DATA,
							method = "Permutation maximum test")
	class(res) <- "htest"
	res
}
#Again we apply the test on the Duncan data:
maxp.test(Duncan$income, Duncan$type, n = length(Duncan))
#This function brings the following error:
##Error in tapply(z, g1, tstat, mu = m) : arguments must have same length
#Will need to get back to this problem once I obtain some proficiency in statistical analysis. 

##Hypothesis testing using a bootstrap:
#Generally, a bootstrap can also be used as a variant of a Monte Carlo test.
#We continue the hypothesis test for a 2-sample test on equal population means. A bootstrap 2-sample test works quite similar to the permutation test. The basic difference is that we draw samples with replacement.
	#1. Draw R bootstrap samples of size n_1 + n_2 = n with replacement. The firest n_1 observation now belongs to sample 1 denoted by X* and the nest of n_2 observations belong to the second sample Y*. 
	#2. For each bootstrap sample estimate population_parameter*_r = f(X*_r) - f(Y*_r), r = 1, ..., B.
	# 3. The p-value is then given by {populationparameter_hat_r >= population parameter_hat}/R, with population parameter_hat estimated from the original samples.
	
#Now let's look in R, this time we keep it simple and do not provide the print output as class htest:
boottest <- function(x, g, n = 10000){
	lg <- levels(g)
	n1 <- length(x[g == lg[1]])
	N <- length(x)
	mdiff <- abs(mean(x[g == lg[1]]) - mean(x[g == lg[2]]))
	z <- double(n)
	for(k in 1:n){
		x1 <- sample(x, replace = TRUE)
		z[k] <- abs(mean(x1[1:n1]) - mean(x1[(n1-1):N]))
	}
	mean(z > mdiff)
}

#The bootstrap test gives a p-value of 0 meaning that no test statistics of the sampled group structure provide a larger value of the test statics as the test statistics obtained from the original data.
Duncan$type <- factor(Duncan$type)
boottest(Duncan$income, Duncan$type)

##A test for multivariate normality:
mvad.test <- function(x, R = 10000){
	n <- nrow(x)
	##test statistics 
	stat <- function(x, N = n){
		cmean <- colMeans(x)
		cvar <- var(x)
		u <- mahalanobis(x, center = cmean, cov = cvar)
		z <- pchisq(u, ncol(x))
		p <- sort(z)
		h <- (2*seq(1:N) - 1) * (log(p) + log(1 + rev(p)))
		A <- -N - mean(h)
		return(A)
	}
	##value of test statistics for original sample 
	A <- stat(x)
	cmean <- colMeans(x)
	cvar <- var(x)
	p <- numeric(R)
	##values of test statistics for draws of mvn
	p <- replicate(R, stat(mvrnorm(n, cmean, cvar)))
	pvalue <- mean(p > A)
	RVAL <- list(statistic = c(A = A),
							method = "A-D radius test", 
							p.value = pvalue)
	class(RVAL) <- "htest"
	RVAL
}

##Size of the test:
#If a well performing random number generator has been selected and if data is drawn from a multivariate normal distribution, then the percentage of rejection should be equal to the chosen significance level.
#This can easily be checked.
#We simulate multivariate normal data with zero co-variances, and we repeat this 1000 times, that is; the Monte Carlo test is applied 1000 times. The significance level is set to be confidence level 0f 0.05. We then check how many times the p-value was smaller than 0.05. The result should be approximately 0.05 if the size of the test is correct.
library(MASS)
set.seed(123)
r <- replicate(100, mvad.test(mvrnorm(100, mu = rep(0,3), Sigma = diag(3)))$p.value)
size <- mean(r < 0.05)
size# It seems that I don't have the proper computer to run these simulations. Will need to use a difference computer for Monte Carlo simulations. 

##Power comparisons:
#Once a test is constructed such that the size fits for different values, significance levels, and dimensions of data sets, it may be compared with other tests. The aim of checking the size of the test was to sample data repreatedly from the null hyporthesis (multivariate normality). Now the aim for power comparisons is to simulate data repeatedly from the alternative hyporthesis. If the data originating from the alternative hyporthesis, of course the rejection rate should be as high as possible.
#For this purpose, data is to be drawn from a multivariate t-distribution. 
library(mvtnorm)
library(ICS)
##Monte Carlo AD test 100 times replicated 
z <- replicate(100, mvad.test(rmvt(30, diag(3), df = 5), R = 100)$p.value)
mean(r < 0.05)
##Skewness test 1000 times replicated 
r2 <- replicate(1000, mvnorm.skew.test(rmvt(30, diag(3), df = 5)) $p.value)
mean(r2 < 0.05)