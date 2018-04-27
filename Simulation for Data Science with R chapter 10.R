### chapter 10 Simulation with Complex Data:
##A model based simple example:
#To simulate a data set with 100 observations from a standard multivariate normal we can use the following code:
library(mvtnorm)
synth <- rmvnorm(100, mean = rep(0,5), sigma = diag(5))
##First three observations 
head(synth, 3)# the results may vary between my findings and the book since the set.seed() function was not activated within the code assembly. 

#If we want to simulate data to be more "similar" to real data, we may use the mean and covariance matrix estimated from such real data. We once again took the Prestige data from the package car, but only the first four columns as the remaining columns are recorded as categorical variables and not continuous. 
data(Prestige, package = "car")
##first three observations of Prestige 
head(Prestige, 3)

#The following code is used to simulate a new data set from MVN(x_hat, S) with x_hat being the column name and S being the estimated covariance of the original data set X:
set.seed(12)
##simulate from multivariate normal
real <- Prestige[,1:4] # I believe this is what the author meant by calling the object real within this function. 
synth2 <- data.frame(rmvnorm(100, mean = colMeans(real), sigma = cov(real)))
#now the moment of truth:
colnames(synth2) <- colnames(real)
##first three observations 
head(synth2, 3)# Cool I obtained the same values as the author. I'm glad that I'm not completely useless at this. 

#Is this result acceptable? Can we use this generation of synthetic data in a simulation study? The answer depends on the aim of the simulation, but most probably the simulation of random numbers was oversimplified. We can see that there are problems with the variable women where the values should be between 0 and 100.
summary(real$women)
summary(synth2$women)
#When we plot two variables from the original and the synthetic data, we can see that the multivariate structure differs. First, the increasing variance due to increasing prestige is not visible for the synthetic data, and in addition, the larger values from the sythetic observations are a bit shifted to the right.
par(mar = c(4,4,0.2,0.2))
plot(prestige ~income, data = real)
points(prestige ~ income, data = synth2, col = "red", pch = 20)
legend("bottomright", legend = c("original/real", "synthetic"), col = 1:2, pch = c(1,20))

#This is due to outliers, which may disturb the mean and covariance estimation. As a way out, we might simulate good data points and bad ones separately.

##A model-based example with mixtures:
#Generally, mixture model might be used if the data has a cluster structure. We only consider this now that there are two mixtures, one containing the good data points and the other one containing potential outliers. We simulate data from both distributions separately. An outlier-robust estimation of the covariance and location can be done with various estimators. We took a simple and fast one, the MCD estimator. If we separate the data generation of outliers and non-outliers we get an even better synthetic dataset. 
library(robustbase)
cv <- covMcd(real)
synth3 <- rmvnorm(100, mean = cv$center, sigma = cv$cov)
par(mfrow = c(1,2), mar = c(4,4,0.2,0.2))
plot(prestige ~ income, data = real)
points(prestige ~ income, data = synth3, col = "red", pch = 20)
##add outliers 
rmd <- mahalanobis(real, center = cv$center, cov = cv$cov)
##outliers defined by large mahalanobis distances 
out <- rmd > qchisq(0.975, ncol(real) - 1)
cv_good <- covMcd(real[!out, ])
##simulate good points 
synth3_good <- rmvnorm(100, mean = cv_good$center, sigma = cv_good$cov)
cv_out <- covMcd(real[out, ])
##simulate outliers 
synth3_out <- rmvnorm(100, mean = cv_out$center, sigma = cv_out$cov)
plot(prestige ~ income, data = real)
points(prestige ~ income, data = synth3_good, col = "red", pch =20)
points(prestige ~ income, data =synth3_out, col = "red", pch = 20)
 
#Of course, we may also use other distributions than the multivariate normal, such as a multivariate Cauchy or multivariate t-distribution. An alternative to create data sets including outliers is the barrow wheel contamination setting. Here, outliers are generated from a distribution that could create a large shape bias. We will not go into too much details on this approach but refer to the R package robustX that contains functionality on it.

##Model based approach to simulate data:
#We may also use a model-based approach. To simulating education, we can simply regress education against a set of well-chosen predictors. For simplicity in explaining this issue, we will take all other variables as predictors without interactions. We can take the fit on the real data and then ust eh model to replace eduction and add some errors based on the residuals of the mode. Remember, errors were also added in Chapter 9.
synth4 <- real
lm1 <- lm(education ~ ., data = real)
synth4$education <- predict(lm1, synth4[,2:ncol(synth4)]) + sample(residuals(lm1))
#We can do this for all variables. For example, we can take the second variable of the Prestige data as response and use the rest as predictors. Again, we fit the model on the real sample and make the fit on the synthetic data:
p <- ncol(real)
for(i in 1:ncol(real)){
	df <- real[, i]
	df <- cbind(df, real[,-i])
	colnames(df)[1] <- "response"
	lm1 <- lm(response ~., data = df)
	lm1 <- lm(response ~ ., data = df)
	synth4[,i] <- predict(lm1, synth4[,-i]) + sample(residuals(lm1))
}

#An implementation of model-based data generation is given in the R package simPop whereby a bunch of models can be used to simulate variables (multi-nominal models, two-step approaches, linear models, regression trees, random forests, probabilistic methods, and so on).

##An example of simulating high-dimensional data:
#In the last example, a specific model fitted on the data was used to simulate new data sets. But for various settings, relationships between variables are expressed by a latent model. This is especially useful for simulation of high-dimensional data, assuming that there is a latent model that may generate the high-dimensional data. 
			#For more information on the equation the author used to describe the latent model consult page 329.

simLatent <- function(n = 200, p = 50, k = 3){
	T <- matrix(rnorm(n * k, 0, 1), ncol = k)
	B <- matrix(runif(p * k, -1, 1), ncol = k)
	X <- T %*% t(B)
	E <- matrix(rnorm(n * p, 0, 0.1), ncol = p)
	XE <- X + E 
	return(XE)
}

#To simulate, for example, 1000 variables on 50 observations with a six component latent model, we can type the following:
x <- simLatent(n = 50, p = 1000, k = 6)
dim(x)

##Simulating finite populations with cluster or hierarchical structures:
#The synthetic data set must be realistic, that is, statistically equivalent to the actual population of interest, and present the following characteristics:
	#The distribution of the synthetic population by region and stratum must be very similar to the distribution of the true population.
	#Marginal distributions and interactions between variables must be accuately represented.
	#Heterogeneities between subgroups, especially regional aspects must be allowed.
	#Cluster and hierarchical structures should be preserved
	#The records in the synthetic population should not be created by pure replication of units.
	
#simPop provides a highly optimized S4 class implementation of various methods in R =, including calibration by iterative proportional fitting and simulated annealing, and modeling or data fusion by logistic regression. 

#There are three broad categories for population simulation techniques:
	#synthetic reconstruction
	#combinatorial optimization
	#model-based generation of data 
	
#model based generation of data is first the deriving of a model of the population from existing microdata, then predicting a synthetic population. So, the fitting is done on existing microdata and the prediction on population. So, the fitting is done on existing microdata and the prediction on population level. In an intial step, the household structure is created by resampling of existing data with probabilities depending on the sampling weights. Additional categorical variables are then simulated using multi-nominal logistic regression models by random draws from observed conditional distributions. In the third step , continuous and semi-continuous variables are generated using regression modelling. 

library("simPop")
data("eusilcS")
dim(eusilcS)

#The number of households is:
length(unique(eusilcS$db030))

#Before simulating variables, we can create (using specifyInput()) an object of class dataObj that will hold all information needed to construct the synthetic population. We can identify the variables providing information on clustering here:households, household size, strata, and sampling weights (variable rb050):
inp <- specifyInput(eusilcS, hhid = "db030", hhsize = "hsize", strata = "db040", weight = "rb050")

#The following is the summary of the specifyInput() data structure:
summary(inp)
print(inp)

#the function simStructure generates the structure of the synthetic population using a replication approach:
synthP <- simStructure(data = inp, method = "direct", basicHHvars = c("age", "rb090", "db040"))

#Categorical variables are simulated using the household structure of the synthetic population and the sample microdata as input, both including in the synthP object of the class simPopObj. In our example, we generate categorical variables on economic status (variable pl030) and citizenship (variable pb220a) by applying simCategorical():
synthp <- simCategorical(synthP, additional = c("pl030", "pb220a"), method = "multinom")

#The following variables: age, category, gender, household size, economic status, and citizenship are used as predictors of personal net income by calling simConinuous:
synthP <- simContinuous(synthP, additional = "netIncome", upper = 200000, equidist = FALSE, imputeMissings = FALSE)

#Basic information on the final population is shown as follows:
synthP

#This population can now be input in design based simulation studies from where samples are drawn from this population.

##Model based simulation studies:
##Latent model example continued:
#We will continue with the latent model from previous example. Such datasets we may use for the comparison of methods. For example, one can mark values to be missing, impute them by suitable imputation methods and evaluate and compare the imputation methods. We can do this by example for a smaller dataset and compare the mean imputation, nearest neighbor imputation, robust model-based impuation, and imputation by mice by using the simple recision-based error criterion based on distances. 
library(mice)
library(VIM)
x <- orig <- simLatent(n = 50, p = 10, k = 6)
##evaluation criteria 
eval <- function(real, imputed, nas){
	sqrt(sum((real - imputed) ^ 2)) / nas
}
set.seed(123)
R <- 100
e1 <- e2 <- e3 <- e4 <- numeric(R)
for(i in 1:R){
	x <- orig
	x[sample(1:nrow(x), 10), 1] <- NA
	e1[i] <- eval(orig, e1071::impute(x), 10)
	e2[i] <- eval(orig, kNN(data.frame(x), imp_var = FALSE), 10)
	e3[i] <- eval(orig, irmi(x), 10)
	e4[i] <- eval(orig, complete(mice(x, m = 1, printFlag = FALSE)), 10)
}
df <- data.frame("error" = c(e1,e2,e3,e4), method = rep(c("mean","kNN", "irmi", "mice"), each = R))

#Boxplots are the most convenient method to compare the distributions of simulation results. See the following graphic for the comparison of methods according to our simple precision error measure:
library(ggplot2)
ggplot(df, aes(x = method, y = error)) + geom_boxplot() + theme(text = element_text(size = 20)) + theme_bw()

##A simple example of model-based simulation:
library(robustbase)
set.seed(123)
x <- rexp(n = 50, rate = 1)
mean(x)
huberM(x)$mu

#on first view this seems to be correct. Both estimators are close to 1, though the Huber mean is closer to 1 than the arithmetic mean. But let's have a look at the result if we repeat the simulation of random numbers and mean calculation 10000 times. 

m <- mean(replicate(10000, mean(rexp(n = 50, rate = 1))))
m # the arithmetic mean of an exponential distribution 
m - 1# the bias of the arithmetic mean

#We can see that the Huber mean of exponential distributed variables is not 1 but around 0.854, and the bias is about -0.146.
mh <- mean(replicate(10000, huberM(rexp(n = 50, rate = 1))$mu))
mh
mh - 1

#This is normal, since the exponential distribution is a non-symmetric distribution , so the comparison with the Huber mean was probably not the best choice. Next we want to have a look at the converage of the arithmetic mean estimator. Each simulated random sample leads to a different estimation of the population mean. We may wonder how many intervals contain the true value of the population mean = 1? The function below estimates the confidence intervals for random numbers of an exponential distribution.
set.seed(123)
alpha <- 0.05
ci <- function(x, z = qnorm(1 - alpha / 2)){
	s <- rexp(n = 50, rate = 1)
	m <- mean(s)
	se <- sd(s) / sqrt(50)
	ci_est <- c(m - z * se, m + z * se)
	ci_est 
}
ci()

#We will replicate this 100000 times and report how many intervals contain the value of 1:
set.seed(123)
ciR_n <- replicate(100000, ci())
isCovered <- function(x){
	apply(x, 2, function(x){
		if(x[1] > 1 & x[2] > 1) return(FALSE)
		if(x[1] < 1 & x[2] < 1) return(FALSE)
		return(TRUE)})
}
cn <- isCovered(ciR_n)
sum(cn) / length(cn)

#We obtain good coverge of about 0.929. If we assume that our estimator (the arithmetic mean) is related to a t-distribution rather than the normal distribution, we can plug-in the quantiles of a t-distribution:
ciR_t <- replicate(100000, ci(z = qt(1 - alpha / 2, 49)))
ct <- isCovered(ciR_t)
sum(ct) / length(ct)

#The coverage rate increased slightly. We also want to compare it to bootstrap estimates of the confidence intervals.
ci_boot <- function(x, R = 1000){
	s <- rexp(n = 50, rate = 1)
	ci_est <- quantile(replicate(R, mean(sample(s, replace = TRUE))),
	c(0.025, 0.975))
	return(ci_est)
}
ciR_boot <- replicate(1000, ci_boot())
cb <- isCovered(ciR_boot)
sum(cb) / length(cb)

#This also lead to the same conclusions, which we can now compare visually. The following graphic shows the lower and upper bounds of the confidence intervals from our simulation:
df <- data.frame(t(ciR_n))
df <- data.frame(rbind(t(ciR_n), t(ciR_t), t(ciR_boot)))
df$method <- rep(c("normal", "t","boot"), times = c(100000, 100000, 1000))
colnames(df) <- c("lower","upper","method")
library(reshape2)
df <- melt(df)
library(ggplot2)
ggplot(df, aes(x = value, color = method)) + geom_density() + facet_wrap(~ variable) + theme(text = element_text(size = 16))

#We sum up all things learned in this section to carry out the estimation of the population mean of a normal distribution. The following function contains the simulation of data and the estimation part:
simMean <- function(simFun = function(x) rnorm(100)){
	##1000 samples 
	set.seed(123)
	R <- 1000
	m <- list() 
	## 1000 datasets 
	for(i in 1:R){
		m[[i]] <- simFun()
	}
	##estimation 
	df <- data.frame("thetahat" = c(sapply(m, mean), sapply(m, mean, trim = 0.1), sapply(m, median), sapply(m, function(x) huberM(x)$mu)), "method" = rep(c("mean","trim","median","huber"), each = R))
	##summary
	vm <- var(df[df$method == "mean", 1])
	df %>%
		group_by(method) %>%
		summarize("bias" = mean(thetahat) - 0, 
				  "variance" = var(thetahat),
				  "mse" = variance + bias^2,
				  "re" = vm / var(thetahat))
}

#We call the function to get all of the results for the different mean estimators. We can see that the arithmetic mean is the most efficient one with the smallest mean squared error:
library(robustbase)
library(dplyr)
simMean()

#Let's modify this example a bit. We can assume the presence of measurement errors and also assume that the data is sampled from the distribution R of contaminated data. F is typically modeled as a mixture of distrubtions where "E" denotes the contamination level, G is the distribution of the non-contaminated part of the data and H is the distribution of the contamination. 

#We do basically the same as previously mentioned, but using another data generation process with 5 percent outliers included:
set.seed(123)
simMean(simFun = function(){c(rnorm(95), rnorm(5,15))})
#We can see that the mean is highly influenced from these outliers, the MSE is much higher than for the huber mean, the trimmed mean, or the median. So in case of the presence of outliers in a dataset, robust estimators are the preferable choice with the lowest mean squared error for this example. 

##A model-based simulation study:
#In previous chapters, the imputation of missing values was focused on. Also , in this chapter, we have already shown an example where we evaluated impuation methods in terms of precision error. 

#In this section, we will compare some imputation methods again. More precisely model-based simulation, which will be demonstrated using an example of the impuation of missing values in compositional data. The examples enhance the previous examples and - their main aim - is to show the use of the simulation framework and R package simFrame.

#It is important to note that compositional data has no direct representation in the Euclidean space and that their geometry is entirely different, see Aitchison 1986. The sample space of D-part compositions is called the simplex and uitable distance measure is called the Aitchison distance D_A. Fortunately, there exists an isometric transformation from the D-dimensional simplex to the real space R^D-1, which is called the isometric log-ratio transformation. 

#Imputation methods for compositional data, which are implemented in the R package robCompositions. while the package is focused on robust methods, only classical imputation methods are used in this example. The first method is a modification of k-Nearst Neighbor imputation, the second follows an iterative model-based approach using least squares regression, the third is equal to the second except a robjst version is used. Also, the EM algorithm implemented in the mice package is compared. Before any computations are performed, the required packages are loaded and the seed of the random number generator is set for reproducibility.
library(robCompositions)
library(simFrame)
library(mice)
library(mvtnorm)

#The data in this example is generated by a normal distribution on the simplex. A random composition follows that this distribution if, and only if, the vector of ilr-transformed variables follows a multivariate normal distribution on R^D-1 with mean vector and covariance matrix summation symbol. The following commands create a control object for generating 150 realizations of a random variable (For this equation make sure to check the end of page 342). The function isomLRinv regards to the inverse isometric log-ratio transformation. The function DataControl allows us to take control of the data simulation process. 
##data generation:
cnorm <- function(n, mean, sigma) data.frame(isomLRinv(rmvnorm(n, mean, sigma)))
sigma <- matrix(c(1,-0.5,1.4,-0.5,1,-0.6,1.4,-0.6,2), 3, 3)
##data control class 
dc <- DataControl(size = 150, distribution = cnorm, dots = list(mean = c(0,2,3), sigma = sigma))

#Furthermore, a control object for inserting missing values needs to be created. In every variable, 5 percent of the observations are set as missing completely at random.
nc <- NAControl(NArate = c(0.05, 0.1))

#For the two selected impuation methods, the relative Aitchison distance between the original and the imputed data is computed in every simulation run. Important to note is that the results are provided as a vector:
sim <- function(x, orig){
	i <- apply(x, 1, function(x) any(is.na(x)))
	ni <- length(which(i))
	x <- x[,-ncol(x)]
	xMean <- e1071::impute(x)
	xMice <- mice(x, printFlag = FALSE, diagnostics = FALSE, m = 1)
	xMice <- complete(xMice)
	xKNNa <- impKNNa(x)$xImp 
	xLS <- impCoda(x, method = "lm")$xImp
	xLTSrob <- impCoda(x, method = "ltsReg")$xImp 
	c(xMean = aDist(xMean, orig)/ni,
	xMice = aDist(xMice, orig)/ni,
	knn = aDist(xKNNa, orig)/ni,
	LS = aDist(xLS, orig)/ni,
	LTSrob = aDist(xLTSrob, orig)/ni)
}

#The simulation can then be run with the command runSimulation, where all parts are put together in the function arguments - the DataControl object, the NAControl object, and the simulation function that actually includes the call of the estimation methods. Here, 25 datasets are simulated and the estimators are applied to these datasets:
results <- runSimulation(dc, nrep = 25, NAControl = nc, fun = sim)# that's interesting despite the console saying that the impKNNa function was depreciated and the error messages this function still worked. Will need to see if the results are the same as the authors. 

#The results can be inspected using head(), aggregate() or simBwplot():
aggregate(results)# that's interesting my values see to differ from the author's by about 0.1 from xMean to the LTSrob object. I will need to look into what the problems is and also what these values actually mean. 

#A boxplot of the simulation results is presented by the following graphic.
simBwplot(results)#This looks really cool. Will need to read the documentation for this function after this chapter.

#Since the imputation methods in this example are evaluated in terms of a relative distance measure, values closer to 0 indicate better performance. The data simulation was too easy to show differences in the other methods.

#For real life situations it is of interest to also include outliers, since they are present in virtually every dataset.
dcarc <- ContControl(target = c("X1"),
			epsilon = c(0.01,0.03,0.05,0.1),
			dots = list(mean = 150, sd = 1), type ="DCAR")
			
results <- runSimulation(dc, nrep = 3, NAControl = nc, contControl = dcarc, fun = sim)

#Then we plot the results in the following graphic with the same function as earlier. 
aggregate(results)
simBwplot(results)

#Clearly, the iterative model-based procedure leads to better results than the modified k-NN approach with respect to the relative Aitchison distance for both rates of missing values and all contamination levels. This is not a surprising result, as the latter is used as a starting point in the iterative procedure. Mean impuation performs the worst, mice is slightly worse than the EM algorithm. For serious evaluation of the impuation methods, however, other criteria also need to be taken into account, for example, how well the variability of the multivariate data is reflected.

#We will now leave almost everything unchanged for a new simulation. The new simulation function should now compare covariances instead of distances.
sim2 <- function(x, orig){
	rdcm <- function(x, y){
		ocov <- cov(isomLR(x))
		rcov <- cov(isomLR(y))
		return(frobenius.norm(ocov-rcov)/frobenius.norm(ocov))
	}
	i <- apply(x, 1, function(x) any(is.na(x)))
	ni <- length(which(i))
	x <- x[,-ncol(x)]
	xMean <- e1071::impute(x)
	xMice <- mice(x, printFlag = FALSE, diagnostics = FALSE, m = 1)
	xMice <- complete(xMice)
	xKNNa <- impKNNa(x)$xImp 
	xLS <- impCoda(x, method = "lm")$xImp
	xLTSrob <- impCoda(x, method = "ltsReg")$xImp 
	c(xMean = rdcm(xMean, orig),
	xMice = rdcm(xMice, orig),
	knn = rdcm(xKNNa, orig),
	LS = rdcm(xLS, orig),
	LTSrob = rdcm(xLTSrob, orig))
}

#Now let's run the simulation again:
library(matrixcalc)
results <- runSimulation(dc, nrep = 3, NAControl = nc, contControl = dcarc, fun = sim2)
aggregate(results)
simBwplot(results)

#Again, we can see that the robust EM approach (LTSrob) from the package robCompositions performs the best for this error measure. 

##Design-based simulation:
#Design-based simulations are particularly important when the selection probabilities for statistical units of a finite sampling frame are not equal, that is, when samples are drawn with a complex sampling design. this primarily relates to any sampling from finite populations, for example, samples drawn from a population register. 

#The costs of a sample survey can be reduced if the sample is drawn with a certain complex sampling design. For example, for poverty measurement, a household with a single parent and children might be included with a higher probability than a household with another composition of household members, because it's likely that the single parent household is poor. 

##Simulation of the synthetic population:
#it is most convenient to use the R package simPop to simulate a population. 

#We can assume that such a population already exists, and use the one which is availabel in the R package simFrame, simulated with the R package simPop.
data("eusilcP")
colnames(eusilcP)
#This population is smaller than the real population but for demonstration issues it is perfectly suited. 
??eusilcP

##Estimators of interest:
#Next, we define the estimators of interest. In our case, this is the Gini coefficient. We also want to compare the classical estimator of the Gini coefficient with the Hill estimator and the robust estimation of the Gini using a robust weighted partial density component estimator.
#Then the function to be run in every iteration is defined, we will call it sim. Its argument k, determines the number of households whose income is modeled by a pareto distribution. Since the Gini coefficient is calculated based on an equalized household income, all individuals of a household in the upper tail receive the same value. 

sim <- function(x, k){
	require("laeken")
	x <- x[!is.na(x$eqIncome),]
	##classical Gini 
	g <- gini(x$eqIncome, x$.weight)$value
	##Hill estimator 
	eqIncHill <- fitPareto(x$eqIncome, k = k, method = "thetaHill", groups = x$hid)
	gHill <- gini(eqIncHill, x$.weight)$value 
	##partial density component estimator 
	eqIncPDC <- fitPareto(x$eqIncome, k = k, method = "thetaPDC", groups = x$hid)
	gPDC <- gini(eqIncPDC, x$.weight)$value 
	##results as a vector 
	c(standard = g, Hill = gHill, PDC = gPDC)
}

##Defining the sampling design:
#The first parapraph of this section has a very good description of survey statistics. I would recommend reviewing this section later on through my studies.

#the previously defined function sim is used in the following examples, which are designed to exhibit the strengths of the framework. The sampling design is specified by the following function:
sc <- SampleControl(grouping = "hid", size = 1500, k = 100)

#In this basic simulation design, 100 samples of 1500 household are drawn using simple random sampling. In order to change from one simulation design to another, all we need to do is define or modify the control objects and supply them to the funciton runSimulation():
library(laeken)# interesting the gini coefficient function is located in this package. 
set.seed(123)
##fun the simulation 
results <- runSimulation(eusilcP, sc, fun = sim, k = 175)
#In order to inspect the simulation results, methods for several frequently used generic functions are implemented. Besides head(), tail(), and summary() mehtods, a method for computing summary statistics with aggregate() is available. By default, the mean is used as a summary statistic. 

head(results)
aggregate(results)
#the following graphic shows the resulting box plot of the simulation results for the basic simulation design.
tv <- laeken::gini(eusilcP$eqIncome)$value 
plot(results, true = tv)

#While the PDC estimator comes with larger variability, all three methods are on average quite close to the true population value. This is also an indication that the choice of the number of households for fitting the Pareto distribution is suitable.

##Using stratified sampling:
#The most frequently used sampling designs in official statistics are implemented in simFrame. In order to switch to another sampling design, only the corresponding control object needs to be changed. In this example, stratified sampling by region is performed. The sample sizes for the different strata are specified by using a vector for the slot size of the control object. 
set.seed(123)
sc <- SampleControl(design = "region", grouping = "hid", size = c(75, 250, 250, 125, 200, 225, 125, 150, 100), k = 100)
##fun new simulation 
results <- runSimulation(eusilcP, sc, fun = sim, k = 175)

#Like before, the simulation results are inspected by head() and aggregate(). A plot of the simulation results can be produced as well (it has been skipped for this example):
head(results)
aggregate(results)
plot(results, true = tv)

##Adding contamination:
#Outliers are virtually present in any data set and it is of high interest to evaluate methods in presence of outliers, that is, to study the invluence of outliers on the estimators. In simFrame, contamination is used in a technical sense here and that contamination is modeled as a two step process, see also Beguim and Hulliger 2008. In this example, 0.5 percent of households are selected to be contaiminated using simple random sampling. The equalized income of the selected households is then drawn from a normal distribution with mean $\m = 500,000$ and standard deviation s = 10000:
set.seed(123)
##define contamination:
cc <- DCARContControl(target = "eqIncome", epsilon = 0.005, grouping = "hid", dots = list(mean = 5e+05, sd = 10000))
##run new simulation 
results <- runSimulation(eusilcP, sc, contControl = cc, fun = sim, k = 175)

#The head(), aggregate(), and plot() methods are again used to take a look at the simulation results. Note that a column was added that indicates the contamination level used.
head(results)

#According to the following graphic, we see that high influence of outliers to the Hill and standard estimator - the gini becomes arbitrary large for those estimators:
tv <- gini(eusilcP$eqIncome)$value 
plot(results, true = tv)

#In other words, the figure shows that such a small amount of contamination is enough to completely corrupt the standard estimation (and also the Hill estimator) of the Gini coefficient. The PDC estimator leads to very accurate results. 

##Performing simulations separately on different domains:
#Data sets from official statistics typically contain strong hetergeneities, therefore indicators are usually computed for subsets of the data as well. So it is often of interest to investigate the behavior of indicators on different subsets in simulation studies. In simFrame, this can be done by simply specifying the design argument of the function runSimulation(). In the case of extending the example from the previous section, the framework then splits the samples, inserts contamination into each subset and calls the supplied function for these subsets automatically. 
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid", size = c(75,250,250,125,200,225,125,150,100), k = 100)
cc <- DCARContControl(target = "eqIncome", epsilon = 0.005, grouping = "hid", dots = list(mean=5e+05, sd = 10000))
results <- runSimulation(eusilcP, sc, contControl = cc, design = "gender", fun = sim, k = 125)

tv <- simSapply(eusilcP, "gender", function(x) gini(x$eqIncome)$value)
plot(results, true = tv)

##Inserting missing values:
set.seed(12345)
sc <- SampleControl(design = "region", grouping = "hid", size = c(75,250,250,125,200,225,125,150,100), k = 50)
cc <- DCARContControl(target = "eqIncome", epsilon = c(0, 0.005, 0.01), dots = list(mean = 5e+05, sd = 10000))
nc <- NAControl(target = "eqIncome", NArate = c(0, 0.05))
results <- runSimulation(eusilcP, sc, contControl = cc, NAControl = nc, design = "gender", fun = sim, k = 125)
aggregate(results)
