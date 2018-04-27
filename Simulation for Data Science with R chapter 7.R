### chapter 7 Resampling Methods:
##the bootstrap:
#The bootstrap is the most popular resampling method to express the uncertainty of an estimate; in other words, to estimate the variance of an estimated statistic of interest. 

#In the following section, we will show with a motivating example that we get basically the same results with the boostrap in comparison to analytical solutions. We will then explain the fundamental concept of the boostrap and answer the question of why we can do an impossible task related to statistical inference iwth the bootstrap. Afterwards, we will discuss the practical application of the bootstrap in detail using R.

##A motivating example with odds ratios:
#Illustration of the bootstrap method of resampling with the New York Times front page article on how taking aspirin cuts heart attack risks.
#The odds ratio of the two components is the following:
(104/11037)/(189/11034)

#A header in the newpaper could possibly be: those who take aspirin regularly have only 55 percent as many heart attacks as people who don't take aspirin.

#As statisticians we want to estimate the real population parameter. Of course we are not really interested in the population parameter only, since it is still only a point estimate of the real population parameter. If we conducted the study again and collected new data, we would get another result.

#We are interested in the accuracy/variability/uncertainty of the population parameter 0.55 (statistical inference).

#But how do we calculate the confidence interval for the population parameter for this sample.
#For a very interesting take on finding the confidence interval for this experiment make sure to take a look at page 206.

#With 95 percent probability, we cover the actual parameter with this interval:
dat<- matrix(c(104, 11037, 189, 11034), 2,2, byrow = TRUE)# through this line of code, the author is only recreating the table of the study as a matrix in place of a data frame.
dat
library(grid)
library(vcd)
confint(oddsratio(dat, log = FALSE))

#The following questions remain unanswered:
	#How do we know that the formula for estimating the KI performs well? Where can we find the proof that this formula is valid?
	#Are there better analytical estimates of the confidence interval available?
	#Do we have simpler methods for the determination of the confidence interval?

#The first question can be answered by some literature search. It turns out that the analytical expression to estimate the confidence interval of the odds ratios is an approximation. When looking at the literature, one can see that there are another 20 different formulas for estimating the confidence intervals of the odds rations; in some cases, other formulas provice better results. If one looks carefully at the literature, it will take time be get enlightened and still it would be difficult to decisde with formula to take. 

#The bootstrap for the aspirin example could be constructed as follows:
#The first group consists of 104 ones and 11037 - 104 zeros, the second one of 189 ones and 11034 - 189 zeros. 

#Bootstrap sample: We draw from the first group a sample of size 11037 with replacement, and from the second a sample of size 11034 with replacement.

#Bootstrap relication: the bootstrap replication of population parameter_or is now:
	#population parameter_or = Ratio of ones in first group/ Ratio of ones in the second group 
	
#We repeat this process (drawing bootstrap samples and calculating the corresponding bootstrap replicates), for example, 10000 times and get 10000 bootstrap replicates. The 95 percent confidence interval is determined by the 0.025 and 0.975 quantiles of the distribution of the bootstrap replicates.

#We can do this practically in R using the following code. Note that this example could be implemented more efficiently by using the prob parameter in the sample function; for didactical reasons, we initialize the whole vector of TRUE (heart attack) and FALSE (No heart attack). 

##original surveyed data:
s1 <- rep(c(TRUE, FALSE), times = c(104, 11037))
s2 <- rep(c(TRUE, FALSE), times = c(189, 11034))
##function for drawing a bootstrap sample 
##and estimating the bootstrap replicate
boot_aspirin <- function(s1, s2){
	##odds ratio:
	sum(sample(s1, replace = TRUE)) / sum(sample(s2, replace = TRUE))
}

##10000 draws and replicates 
boot_repl <- replicate(10000, boot_aspirin(s1, s2))
##Confidence interval 
quantile(boot_repl, c(0.025, 0.975))

#For this example, the confidence intervals by bootstrap are very close to the one estimated previously from the analytical method. The estimation of confidence intervals using bootstrap was data-based, without preconditions (except the assumption that a good random number is chosen) and done in a very intuitive manner without mathematics.

##Why the bootstrap works:
#You want to ask a question of a population but you can't since you don't know the whole population. Therefore, you take a sample and ask the question of it instead. The idea is thus that we have just one vector x. Note that, for simplicity, we just work with a vector of values, but everything holds true for a dimensional dataset X as well. When we estimate a population parameter (a statistic) population parameter = f(X) from this empirical dataset, we usually don't have a clue about the uncertainty of variability of this estimate. But we know that there must be an uncertainty since - in theory - if we were to conduct another second sample data set empirically, say x_2 from the same population, the resulting estimate population parameter_2 = f(x_2), would differ from the population parameter. In other words, how confident are you that the sample answer is close to the population?

	#Get information about everything (can be costly for finite population and can cost up to infinity for infinite populations)
	#Get information repeatedly. Let us give further comments on these suggestions. If you don't know the population, one way you might learn about the uncertainty of your population estimates is to take samples from the population again and again. For each of the samples taken, we would apply the same estimators to obtain the statistic of interest for each of these samples. We then would see how variable the sample answers tend to be.
	#The bootstrap method. Replicating the sample dataset multiple times as a means to create a population distribution. Through sampling the data multiple times you are effectively bringing about it's distribution shape as a psuedo-population (assuming of course that the data set is the best sample one can carry out).
	
##A closer look at the bootstrap:
#The basis is to draw a random sample of size n from a probability distribution F; each draw element is included in the sample with probability 1/n. In other words, a random sample is given by: x_i ~ F(i.i.d.) with i = 1, ...n values drawn from the population. 

#The distribution of the bootstrap samples converges almost surely to the actual probability distribution. This works because:
	#The fact that for bootstrap sample the selection probability of an observation is still 1/N
	#The (strong) law of large numbers 
	#The central limit theorem - the bootstrap samples mimic the population.
	
#A bootstrap sample is usually of size n, the same size as the sample. A question could be: why not take bootstrap samples of size m != n? The answer is easy. Most statistic/ estimates are dependent on the smaple size. The bootstrap distribution would just have potentially more spread when m < n  and less spread if M > N, which would lead to an over- or underfitted estimated variance.

##The plug-in principle:
#The bootstrap is based on the plug-in principle, that is, if something is unknown, then substitute an estimate for it. This principle is a familiar approach in statistics. For example, in chapter 4, simulation of random numbers we already plugged in the empirical standard deviation for the estimation of confidence intervals, since we don't know the true variance, but the variance is noted in the formula for the confidence interval. With the bootstrap, we go a step further. Instead of plugging in an estimate for a single parameter, we plug in an estimate for the whole distribution. 

#What is meant by the plug-in principle is to do nothing more than just replace the population parameter = t(F) with the distribution function of the empirical distribution F_hat = t(F_hat). Note: often one write the parameter population parameter directly as a function of the distribution of population parameter = t(F) instead of population parameter = t(X)). It is assumed that population parameter can be calculated by the function t(F). In the following section, the bootstrap is used to estimate standard errors and bias. 

##Estimation of standard errors with bootstrapping:
#Let's consider the following toy example with the following seven numbers. We use such small sample and toy example just to explain the bootstrap in R. The estimator of interest should be the arithmetic mean in our example. 
x <- c(5,7,8,2,15,12,3)
#We next define the boostrap sample. A bootstrap sample is a random sample. One bootstrap sample is given by:
##For reproducibility we use a seed:
set.seed(123)
##bootstrap sample (with replacement)
s1 <- sample(x, replace = TRUE)
s1
#for my sample the 7 was repeated two times and the 12 was omitted as a result. This can happen since we sample with replacement, meaning that if we draw a number, it will be replaced so that the next draw is again from  the numbers in object x.

#Of course, we can repeat this to get another bootstrap sample, which of course differs with a high probability from the previous bootstrap
s2 <- sample(x, replace = TRUE)
s2
#The arithmetic mean of our toy sample is:
mean(x)# the result was 7.428571
#Take a look at page 213 for the mathematical proofs of the bootstrap method at this particular junction. 

#For our bootstrap samples drawn, the arithmetic means (= bootstrap replicates are as follows):
mean(s1)
mean(s2)

#Let's take a larger sample, for example, the prestige data from the R package car. One variable is related to income. Again, let's take a bootstrap sample and estimate the bootstrap replcate. 

#But first, look at the statistic of interest from the sample. Let's assume that the statistic of interest is the arithmetic mean.
library(car)
data("Prestige")
dim(Prestige)#According to the results the Prestige data set only has 102 observations and 6 columns. Meaning that this is the perfect data set for the bootstrap method (since the sample size is too small to be considered normally distributed).
set.seed(123)
mean(Prestige$income, replace = TRUE)# the result was 6797.902
##A bootstrap replicant of the mean is:
mean(sample(Prestige$income, replace = TRUE))# the result was 6393.882

#Another can be achieved by repeating the previous line of code (without the seed):
mean(sample(Prestige$income, replace = TRUE))# the result was 6662.529

#The crucial thing is now how to express the variability of our statistic of interest. Before we discuss confidence intervals, we discuss how to estimate the standard error of an estimator. The bootstrap algorithm to estimate the standard error of an estimated parameter is as follows:
	#Select independent bootstrap samples 
	#Calculate the bootstrap replications for each bootstrap sample
	#estimate the standard error se_F on the standard deviation of replications
	#For more of than over view of the equations for bootstrapping look at page 214.
	
#An illustration of the non-parametric bootstrap in the original definition of the bootstrap is given in Efron and Tishirani (1993). We want to go a step further and show the bootstrap to estimate the standard error.

#At the top of figure 7.2, we see a whole population. this population is unknown but we have one sample from it, say x of size n. From this sample, we draw bootstrap samples with replacement. All in all, we draw bootstrap samples of size n each. We obtained a distribution of bootstrap replicates. From the bootstrap replicates, the standard error SE*_hat can be estimated. 

#To obtain stable and good results, one needs a certain number of bootstrap replications. In general, this course always depends on the problem and the distribution of the data. Basically, we often obtain stable results (when re-running the bootstrap, the same results obtained) with just 25 bootstrap replications, and 50 replications give very stable ressults for the standard error. Often 100 replications are chosen, more than 200 replications bring mostly no more profit. 

#In general, when other estimates are of interest, for example, the confidence intervals or model-based results, many more replications are needed. For estimating confidence intervals, more than 5000 replications are useful. We see this in the section "confidence intervals by bootstrap". 

##An example of a complex estimation using the bootstrap:
#It is often very hard to express standard errors in an analytical manner, even for simple estimators. We will show an example for the standard error for the (robust) correlation coefficient. The robust correlation measure can be achieved with the Minimum Covariance Determinant (MCD) algorithm. This is a rather complex estimator, where it is a huge challenge to estimate standard errors using a formula. We will see by using the bootstrap this is as easy as estimating the standard error for the arithmetic mean.

#In R the MCD-based correlation can be calculated as follows. For simplicity, we only estimate the robust correlation between income and prestige.
library("robustbase")
library(car)
df <- Prestige[,c("income", "prestige")]
##robust MCD - based covariance:
covMcd(df, cor = TRUE)$cor

#The following legitimate question can be asked: How can one estimate the standard error for the correlation coefficient? Some further questions arise:
	#Do you know how to estimate the standard error for the classical correlation coefficient (Pearson) analytically? If so, do you know the formula for the standard error of the correlation coefficient if...
	#The data does not follow a multivariate normal distribution?
	#Can you provide an appropriate analytical approximation?
	#Can you calculate it for this example?
	#Do you know it for the MCD-based correlation coefficient?
	
#The answers are probably the following:
	#Yes, when searching in Google or in a good book.
	#No, I suppose there are only estimates for normally distributed data.
	#No, but presumably this has been dealt with by someone else.
	#Yes, quite easily with the bootstrap.
	
#then let's do it. We sample (with replacement) the Prestige data using sample() and estimate the MCD based correlation with the function covMcd(). We replicate this 200 times and estimate the standard error:
set.seed(1234) #for reproducibility (seed)
## standard error with bootstrap:
sd(replicate(200, covMcd(df[sample(rownames(df), replace = TRUE), ], cor = TRUE)$cor[1,2]))

#In R, we can also make use of the function boot. We see that we get very similar results for the standard error. The fundamental concept of the boot function is to express how bootstrap samples should be drawn. This is done by writing our own function with a function argument regarding the data and another one about the index that should be sampled with replacement. 
library(boot) 
##function for bootstrapping in boot
cr <- function(d,w) covMcd(d[w,], cor = TRUE)$cor[1,2]
##application of the boot function 
boot(data = df, statistic = cr, R = 200)# I obtained the same answers as the author but I really don't know how the author came up with the former function. Will need to go back to this function (or rather) method later on through my studies. 

##the parametric bootstrap:
#Generally speaking, when we have properly specified model, simulating from the model often provides reliable estimates even with smaller number of replicates than the non-parametric bootstrap. However, if the parametric model is mis-specified, the solution converges to the wrong distribution. Thus, when using the parametric bootstrap, the assumptions must hold.

#We would like to show an application of the parametric bootstrap to show the properties of this method. Suppose that we have information that allow us to conclude that the two variables income and prestige in the dataset Prestige are drawn from a bivariate normal distribution - this is the model here to be assumed. We now estimate the mean and covariance from the empirical data and draw from the theoretical normal distribution with the corresponding parameter values of the empirical data:

##MASS packaged needed for drawing random numbers from multivariate normal distributions.
library(MASS)
## parameters from empirical data (income and prestige)
ml <- colMeans(df)
m2 <- cov(df)
##number of observations 
n <- dim(df)[1]
##number of observations
dim(df)[2]
##parametric bootstrap 
parboot <- replicate(200, covMcd(mvrnorm(n, mu = ml, Sigma = m2), cor = TRUE)$cor[1,2])
##standard error:
sd(parboot)

#If we want to look at the computational speed also in comparison to the computation time of the non-parametric bootstrap, we increase the number of replicates and use the classical estimate of correlation.
##parametric bootstrap 
system.time(sd(replicate(5000, cor(mvrnorm(n, mu = ml, Sigma = m2))[1,2])))

##non-parametric bootstrap 
system.time(sd(replicate(5000, cor(df[sample(rownames(df), replace = TRUE), ])[1,2])))

#We see that there is not as much difference in computation time for the parametric and non-parametric bootstraps in this example. However, in the literature, often it is said that the difference is about the stability of results; in other words, it is often mentioned that if well specified, the parametric bootstrap provides stable results for the standard error for smaller sample sizes. however, this must not be true in the general case, and basically this is not even true for our example. We replicate the estimation of the standard error 20 times with R = 50 each and look at the range of results.

##parametric bootstrap 
range(replicate(20, sd(replicate(50, cor(mvrnorm(n, mu = ml, Sigma = m2))[1,2]))))
#The results for the standard error are 0.04194079 and 0.06062707 respectively.

##non-paramtric bootstrap
range(replicate(20, sd(replicate(50, cor(df[sample(rownames(df), replace = TRUE),])[1,2]))))
#The results for the standard error using this method are 0.03479239 and 0.05161005 respectively.

#We see that the standard errors differ slightly.

#Let's finally compare the results from the non-parametric and the parametric bootstrap. We draw 1,000 bootstrap samples. 
##parametric bootstrap 
phoot <- replicate(1000, cor(mvrnorm(n, mu = ml, Sigma = m2))[1,2])

##non-parametric 
npboot <- replicate(1000, cor(df[sample(rownames(df), replace = TRUE), ])[1,2]) 
mi <- min(phoot, npboot) 
ma <- max(phoot, npboot)

#Visually, these differences can be shown, for example, with histograms of the distribution of the bootstrap replicates. Now we plot these values. The plot can be seen with the following graphic. 
par(mfrow = c(1,2), pty = "s")
hist(npboot, main = "non-parametric", xlab = "1000 bootstrap replicates", xlim = c(mi, ma), breaks = 25)
hist(phoot, main = "parametric", xlab = "1000 bootstrap replicates", xlim = c(mi, ma), breaks = 25)

#from the following graphic representation, we see that the distribution of bootstrap replicates is not very different for the parametric and non-parametric bootstraps in our example. 

##Estimating bias with bootstrap:
#Previously, the standard error was used as a measure of the accuracy of an estimator population parameter. We now want to look at the bias, the difference of the estimator population parameter and the parameters to be estimated the population - we want to look at systematic distortions of the estimator population parameter.

#The reasons for bias in the data can be very different: systematic errors in registers, poor sampling design, heavy earners not reported, outliers, robust estimates of sampling means or totals in complex sampleing designs, and so on. 

#The bias of population parameter = f(x) is the deviation from the actual parameter of the population, that is, population parameter = bias_population parameter = E(pop_hat) - pop. Since the population parameter is generally unknown, the bais can usually only be expressed using resampling. In the following, we only concentrate on this mathematical bias and do not consider any other kind of kas (such as systematic bias from data collection).

#The bootstrap estimate of the bias is based on bootstrap replications:

#We again use the Prestige data and select the varaible income. The parameters to be estimated of the population is the coefficient of variation, v = s_hat/x_sample with s_hat the estimated standard deviation and X_sample the arithmetic mean. this can be done in R like this:
x <- Prestige[, "income"]
v <- function(x) sd(x) / mean(x)
v(x)# The variation is 0.624593

#1000 bootstrap sample can be drawn easily, and the bootstrap replicates are easy to calculate:
vboot <- replicate(1000, v(sample(x, replace = TRUE)))

#The bias is then given by the mean of bootstrap replicates estimate and the sample estimate:
vbias <- mean(vboot) - v(x)
vbias#The result is -0.01215573

#A bias-corrected estimator is then given by:
v(x) - vbias
#(side note from the author) Just being aware that a statistic is biased may help you to proceed more appropriately.

##confidence intervals by bootstrap
#The easiest method to estimate confidence intervals is just by plugging in the estimate of the standard error by bootstrap in the analytical formula for the confidence interval. But we will see that this is not the best idea.

#From the previous example, one can determine this confidence interval as follows:
cat("CI(e): [", v(x) - vbias - qt(0.975, length(x) - 1) * sd(vboot), ", ", v(x) - vbias + qt(0.975, length(x) - 1) *sd(vboot), " ]\n")
#Really cool line of code that organizes the Confidence intervals in a coherent fashion. The results are 0.5059089 and 0.7675889 respectively.

#However; expect from the bias correction these confidence intervals are always symmetric. However, one big advantage of the bootstrap is that the intervals might not be symmetric. For example, think of a ratio estimator, for example, the ratio of votes for a small party according to all votes in elections, and say this party has 1.5 percent of votes. With classical methods and also with this approach for estimating the confidence interval with bootstrapping, it is likely that the left side of the symmetric confidence interval is negative. 

#Especially if the bootstrap distribution of an estimator is symmetric, then percentile confidence intervals are often used. Efron's percentile method for bootstrap confidence intervals is the most popular method and it has good properties when the number of bootstrap replicates is large.

#Insteap the classic estimate of the standard error is replaced with the estimate obtained by bootstrapping, the lower and upper percentile of bootstrap distribution is used. 

#For the Efron method of percentile confidence intervals using the bootstrap review page 223. I'm currently unable to understand this formula at the moment. 

#If we choose, for example , R = 100 and for the significance level = 0.05, the percentile confidence interval is determined by the 2.5th and 975th values of the 1,000 sorted bootstrap replicates. The reason why I changed the 25th percentile to 2.5 percentile is that I believe that the author forgot that halving 0.05 confidence interval to 0.025 and 0.025 respectively.

#For our example, this confidence interval is determined as follows:
cat("CI(p): [", quantile(vboot, 0.025), ", ", quantile(vboot, 0.975), "]\n")# Interesting the results are 0.4816875 and 0.734903 respectively. 

#The bootstrap Bias Corrected alpha (BCa) confidence interval method makes use of another kind of bias estimate, the fraction of the bootstrap distribution that is < population parameter. The confidence interval of the BC_alpha method is also based on the percentiles. These percentiles depend on two numbers, the acceleration a_hat and the bias correction z_hat_0
#the following formulas in this section of the book are a little over my skill level. Will need to go back to page 224 once I obtain the prerequisite statistical and calculous knowledge.

#Next let's compare the confidence intervals of the percentile method with those of the BC_alpha method, and then compare it to all mentioned methods based on some simulated toy data:

x <- c(rnorm(100), rnorm(10,10))
##non-parametric bootstrap replicates:
mb <- replicate(1000, mean(sample(x, replace = TRUE)))
##percentile method:
cat("\nCI(perc): [", quantile(mb, 0.025), ", ", quantile(mb, 0.975), "]\n")#the result is 0.48249 in the lower bound and 1.589897 in the upper bound.

##BC_alpha 
library(bootstrap)
b <- bcanon(x, 1000, mean, alpha = c(0.025, 0.975))
cat("\nCI(BCa): [", b$confpoints[1,2], ", ", b$confpoints[2,2], "]\n")
#For this situation the CI(BCa) was calculated at 0.5069378 and 1.655714 respectively. It's important to keep in mind that the sample for the BC_alpha and the non-parametric methods are all randomly generated. This means that values my vary between situations. 

#You can see from the following values that the differences are not dramatic. This is also true for other distributions, but it may depend on the parameter of interest and sample size as well as the number of bootstrap replicates. A non-smooth estimator is usually more problematic as the arithmetic mean in the previous example, expecially for a resampling method that we present in the following section - the jackknife.

##the jackknife:
#The jackknife is - like the bootstrap - a resampling method. The jackknife can be used to determine bias and standard error of estimators. It is simpler and faster than the bootstrap, since we do not draw new (bootstrap) samples, but we leave out one value from the original sample (for each jackknife sample). We just make estimations with one observation excluded. 
#This technique can be used as an easy to use and fast to calculate tool that can solve a variety of problems. While the jackknife was very popular in the past because of its simplicity and fast computation, it generally has lower quality than the bootstrap, and it should be used in only rare specific cases. 

#In the following, we show results from both classical jackknife and jackknife using jackknife pseudo-values.

#For the last example from the section about bootstrap, we now apply estimattes with the jackknife in R. We use a toy data set with only few numbers. With this dataset, we show the problems of the jackknife, especially later when we estimate non-smooth statistics such as the median. Before we come back to the variation coefficient. 

##toy data:
x <- c(1,2,2,2,2,2,7,8,9,10)
##remember this is the variation coefficient.
v <- function(x) sd(x) / mean(x)
##initialization 
n <- length(x)
vjack <- rep(0, n-1)
vpseudo <- rep(0, n)
##leave one out jackknife 
for(i in 1:n){
	vjack[i] <- v(x[-i])
}
##jackknife pseudo values 
pseudo <- n * v(x) - (n - 1)*vjack
cat("\nKI(pseudo): [", mean(pseudo) - qt(0.975, n - 1) * sd(pseudo)/n, ", ", mean(pseudo) + qt(0.975, n - 1) * sd(pseudo)/n, " ]\n")

##confidence interval with classical jackknife:
se2 <- sqrt(((n-1)/n) * sum((vjack-mean(vjack))^2))
jbias <- (n-1) * (mean(vjack) - v(x))
cat("\nKI(jse): [" , v(x) - jbias - qt(0.975, n - 1) * se2, ", ", v(x) - jbias + qt(0.975, n - 1) * se2, "]\n")

#From this example, we see that we may underestimate the confidence intervals especially by using jackknife pseudo values, because we have a very small sample and, naturally, the confidence interval should thus be larger. In comparison, we show the results of a bootstrap that leads to broader confidence intervals.
quantile(replicate(1000, v(sample(x, replace = TRUE))), c(0.025, 0.975))

##Disabvantages of the jackknife:
#The jackknife (later we see another kind of jackknife) is a very simple procedure to often recieve good approximations, for example, for the bias and the standard error. However, the jackknife often does not converge to the true standard error, especially for non-smooth estimators. We give an extreme example in the following to see how the jackknife, may underestimate the variation of the parameter. We use the same toy data set as before, but instead of the variation coefficient , we estimate a non-smooth estimator such as the median.

##Sample estimate:
median(x)
##non-parametric bootstrap:
qu <- quantile(replicate(1000, median(sample(x, replace = TRUE))), c(0.025, 0.975))
cat("\nCI(boot): [", qu[1], ", ", qu[2], " ]\n")

##jackknife, initialization:
n <- length(x)
jack <- rep(0, n -1)
pseudo <- rep(0, n)
for(i in 1:n){
	jack[i] <- median(x[-i])
}

##jackknife pseudo values approach:
pseudo <- n * median(x) - (n -1) * jack
cat("\nCI(pseudo): [" , mean(pseudo) - qt(0.975, n - 1) * sd(pseudo)/n, ", ", mean(pseudo) + qt(0.975, n - 1) * sd(pseudo)/n, "]\n")# weird the author was right the /n needed to be included for the line of code to work. Will need to look into what the /n syntax means. 

##classical jackknife:
se2 <- sqrt(((n - 1)/ n) * sum((jack - mean(jack)) ^2))
jbias <- (n - 1) * (mean(jack) - median(x))
cat("\nKI(jse): [" , median(x) - jbias - qt(0.975, n - 1) * se2, ", ", median(x) - jbias + qt(0.975, n - 1) * se2, "]\n")

#We see that the jackknife completely underestimates the confidence interval: it is of zero length. The toy data set used had too many 2s, so when leaving out one value, the median is still 2. Thus the confidence interval estimated by the jackknife must be of zero length since all jackknife repllicates have a value of 2. Especially for non-smooth estimators, the jackknife may underestimate the variability of an estimator. 

##The delete d- jackknife:
#For a very good over view of this method look at page 230. According to the author, for this method to work in place of obtaining n from subtracting one observation you are subtracting n by d. 

#Let's show this with the delete-2 jackknife. The aim is to create a matrix of (n over d) combinations, in our case (n over 2), and apply a delete-d jackknife to that. Again, we want to estimate the median from the previous example:

#Before I continuou on I still dont' know what d is. Will need to look this up later on.

##all combinations:
co <- combn(10, 2)
##first 6 out of 45
co[,1:6]
##delete-2 jackknife replicates
jack_d <- apply(co, 2, function(i) median(x[-i]))
##standard error 
n <- length(x)
r <- 2/n
##n over 2
nd <- choose(n, 2)

##inflate factor 
fac <- r /nd
m <- mean(jack_d)
## standard error 
se_d <- sqrt(fac * sum((jack_d - m) ^2))
##confidence interval 
cat("\nKI(jse): [" , median(x) - qt(0.975, n - 1) * se_d, ", ", median(x) + qt(0.975, n - 1) * se_d, "]\n")

#the confidence intervals by the delete-2 jackknife seems reasonable. However, in general, the delet-d jackknife is not an efficient method. We investigate in a very small data set. For example, the number of possible combinations is for a sample of a slightly larger data set with 45 observations and d equals 10, we will get the following number of jackknife samples.
choose(45, 10)# In other words, the method sometimes generates impractically large numbers. thus the classical and the pseudo jackknife method should be used. 

##Jackknife after bootstrap:
#The jackknife after bootstrap method is a method to estimate the variance of the standard error or, in general the uncertainty of the confidence interval estimate. To do this, one can make another boostrap, but this means that for R= 1000 to estimate the variance of the point estimator of interest and for another 1000 replications to estimate the variance of the variance estimator leads to bootstrap replicates.

#But there is a trick to reduce this number considerably by using a jackknife after bootstrap in a special manner. Basically, we will see that only the information of the bootstrap samples is used. 

#the jackknife after bootstrap plot calculates the jackknife influence values from a bootstrap output object, and plots the corresponding jackknife after bootstrap plot. It is used to determine which observations have a high influence on the bootstrap replicates distribiotn and on se_hat_R 

#Let's use the Prestige data set once again. We again denot the funciton v as the estimate of the coefficient of variation. In order to be able to use the function boot(), we write down the function with two parameters. 
x <- Prestige$income 
v <- function(x, indices){
	x <- x[indices]
	est <- sd(x) / mean(x)
	return(est)
}

#The following is the code that creates a bootstrap after jackknife plot.
library(boot)
bx <- boot(x, v, 2000)
jack.after.boot(bx)

#These two observations - you can see that the data values itself - have by far the largest jackknife values. The horizontal lines indicate the quantile range of the bootstrap distribution, centered with respect to the value of the original data. The points that are connected to these lines denote the estimated (using bootstrapping) quantiles without the respective observations. One can see that especially for observations 2 and 24 the quantiles of the bootstrap replicates get smaller compared to the esitmates when 2 and 24 are not excluded. 

#The jackknife after the bootstrap plot merely gives an indication of which observations have great influence on the variance estimate. 

##Cross-validation:
#Cross validation is a resampling method as well, similar to the jackknife. However, the aim is now not to make inference statsitics but to estimate prediction errors. 

#Cross validation is mainly used for the comparison of methods or to find the optional values of parameters in an estimation model. 

##The basic concept of cross validation:
#the idea of cross-validation is related to estimating the prediction error. In principle, a data set is divided into training and testing data. Based on the training data, parameters are estimated and evaluated on the basis of the test dataset. 

#Ideally, the training dataset contains the current data and the test dataset is determined by new measurements. Often, no new measurements are made for logistical or cost reasons. Then, dividing the current data into test and training data is usually made. 

#On the basis of a simple two dimensional data set and according to the problem of ordinary least squares regression, cross validation and its variants are explained in the following: 

set.seed(12345)
##generate some random data:
x1 <- runif(100, 0, pi)
s <- data.frame(x1 = x1, x2 = rnorm(100, 0, 0.1) + sin(x1))
plot(s, xlab = "x", ylab = "y")
##simple model 
regl <- lm(s[,2] ~ s[,1], data = s)
abline(regl, lwd = 2)
##sinus model 
reg2 <- lm(s[,2] ~ sin(s[,1]), data = s)
f <- function(x, coef) coef[1] + coef[2] * sin(x)
ss <- seq(-0.02, 3.2, 0.01)
lines(ss, f(ss, coef(reg2)), lty = 2, col = "blue", lwd = 2)

##locally reweighted regression 
reg3 <- lowess(x = s[,1], y = s[,2], f = 0.1)
lines(reg3, col = "red", lty = 3, lwd = 2)

##legend for figure 7.7 
legend("bottom", col = c("black","blue","red"), lty = c(1,2,3), lwd = c(2,2,2), legend = c(expression(y = beta[0] + beta[1]*x),
						expression(y = beta[0] + sin(x)),
						"loess, 0.1"))
						
#Really cool representation of what you can do in the realm of statistical transformations. 

#Here, the models have to be evaluated using, for example, cross validation. In general, we are interested in how well we can predict data.

##classical cross validation -- 70/30 method
#As training data, we use as 70 percent of the data previously simulated. This 70 percent is selected randomly. The rest of the data serves then as the test dataset. Based on the training data, the three proposed methods are now applied and the performance of the methods is evaluated by the test dataset. 

#Let's have a look at the toy dataset:
str(s)

#We want to select 70 percent of the observations randomly to serve as training dataset:
##index of training data:
training_ind <- sample(1:nrow(s), 70)
##index of test data 
test_ind <- which(!(1:100 %in% training_ind))

#The model is now estimated on the training data; we can do that using the function lm().
lm1 <- lm(s[training_ind, 2] ~ s[training_ind, 1], data = s)

#For the OLS model the evaluation is easy, we just have to estimate the expected values by y_hat_k = beta_hat_0 + beta_hat_1 * x_k

##expected values:
f <- function(x) regl$coef[1] + regl$coef[2] * x 
##Prediction error, squared sum of expected and observed test data 
error <- sum((f(s[test_ind, 1]) - s[test_ind, 2])^2)
error

#the selection of test and training data and the evaluation may repeat, for example, 1000 times 

#The sum of squared errors with respect to the test data is then distributed for model 2 as follows:

#This basic principle -- the selection of test and training data and evaluation -- is now repeated 1000 times with random division into traing and test data. The sum of squared errors with respect to the test data is then distributed for model 2 (sinus) as follows:
f_me <- function(x) reg2$coef[1] + reg2$coef[2] * sin(x) 
error2 <- sum((f_me(s[test_ind, 1]) - s[test_ind, 2])^2)
error2# Neat I almost got the author's same value for this particular model. the value that I obtained was 0.471989 meaning that I'm about 0.01 off the mark of what the author got for his value. 

f_me <- function(x) reg2$coef[1] + reg2$coef[2] * sin(x) 
error1 <- numeric(1000)
training_ind <- numeric(1000)
n <-nrow(s)
for(i in 1:1000){
	training_ind <- sample(1:n, 70)
	reg2 <- lm(s[training_ind, 2] ~ sin(s[training_ind,1]), data = s)
	error1[i] <- sum((f_me(s[-training_ind,1]) - s[-training_ind, 2])^2)
}
summary(error1)

#The advantages of this cross validation method by splitting into 70 percent training and 30 percent test data are mainly in the ease of use and realtively simple method of selection. However, the disadvantages is that a lot of the data has not been selected to estimate the model (here 30 percent). 

##Leave-one-out cross validation:
#similar to the jackknife, one observation is omitted from the fitting phase, and the model is evaluated based on the omitted observation. This single observation represents the whole test data set. 

#In our example, this would be for our simple model:
n <- nrow(s)
error1 <- numeric(n)
for(i in 1:n){
	reg2 <- lm(x2 ~ x1, data = s[-i,])
	error1[i] <- sum((f(s[i, 1]) - s[i, 2])^2)
}
mean(error1)

##K-fold cross validation:
#here the data is divided in k groups randomly.
#As a measure for the quality of the prediction (the prediction error), we can take the sum of squared residuals in the case of regression analysis. The arithmetic mean of estimates often serves as the prediction error of the model. 

#K-cross validation is best made in the case of regression analysis using the R package cvTools. In the example below, we basically do the same as above, but using 5-cross validation, and in addition we repeat 5-fold cross validation 10 times and report the mean squared error:
library(cvTools)
fit <- lm(x2 ~ x1, data = s)
# perform cross-validation 
cvFit(fit, data = s, y = s$x2, cost = mspe, K = 5, R = 10, seed = 1234)

#The cvTools package makes it handy to select the best method. Let's come back to the dataset Prestige and let us apply some models and different regression methods to it:
library(robustbase)

folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(prestige~ ., data = Prestige)
cvFitLm <- cvLm(fitLm, cost = mspe, folds = folds)
fitLm2 <- lm(prestige ~ income:type + education + women, data = Prestige)
cvFitLm2 <- cvLm(fitLm, cost = mspe, folds = folds)
fitLmrob <- lmrob(prestige ~ ., data = Prestige)
cvFitLmrob <- cvLmrob(fitLmrob, cost = mspe, folds = folds)
fitLmrob2 <- lmrob(prestige ~ income:type + education + women, data = Prestige)
cvFitLmrob2 <- cvLmrob(fitLmrob, cost = mspe, folds = folds)
cvSelect(LS = cvFitLm, LS2 = cvFitLm2, MM = cvFitLmrob, MM2 = cvFitLmrob2)