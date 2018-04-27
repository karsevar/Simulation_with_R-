### chapter 11 System Dynamics and Agent Based models 
#What makes using system dynamics different from other approaches to studying complex systems is the use of feedback loops and stocks and flows. Empirically, a dynamical model is descirbed in a mechanistic manner as input output, a certain king of black box. We consider the change of single objects (agents, automata, individuals), populations, and interactions over time. 

##Agent based models:
#Microsimulation models are favored in the area of demographics for populations forecasts, simulating the spread of diseases, and to forecast social or economic changes. In population statistics three continuous time scales are important, the individual's age, time, and the time that the individual has already spent in a specific state.

#Optionally, a microsimulation starts with a true complete population. Since this is rarely the case that the population is available, a true population must be augmented or even completely simulated. A synthetic population is already simulated in chapter 10 and it should contain information on individuals that is demographically relevant, such as sex, marital status, fertility status, and information on education level. 

#We should first take some notes on fertility and mortality rates.

#For any category that can change over time we basically need a transition matrix or probabilities from a regression model. Zinn 2014 gives the example of the Hadwiger mixture model that can be used to describe the transition rates among women giving birth to a second child. The funciton inputs are age, calendar time, and lastbirth (time since the firstbirth in years).
fert2Rate <- function(age, time, lastbirth){
	a <- ifelse(time <= 2020, 32, 33)
	b <- ifelse(time <= 2020, 6.0, 5.7)
	frate <- (b/a) * (a/age) ^ (3/2) *
				exp(-b ^ 2 * (a / age + age /a - 2))
	frate[age <= 15 | age >= 45 | lastbirth < 0.75] <- 0
	return(frate)
}

#The fertility rate for a 30 year old women that has a three year old baby until the year 2030 is this, according the the simplified model.
fert2Rate(30, 2030, lastbirth = 3)
#The result is 0.1483116

#The older the women the lower the fertility rate.
fert2Rate(40, 2030, lastbirth = 3)
#The result was 0.03874834

#This is just an example of modeling the fertility rate, in the real world the fertility rate will depend on much more covariates such as education level, income, living area, ethnic background, and so on. 

#The Mortality rate will also depend on many covariates. We will give just a simplified version also used in Zinn 2014.
mortRate <- function(age, time){
	a <- 0.0003
	b <- ifelse(time <= 2020, 0.1, 0.097)
	mrate <- a * exp(b * age)
	return(mrate)
}

#The mortality rate of the 40 year old author of this book in the year 2056 is:
mortRate(40, 2056)

#For the initial population of time T_0, we use the one simulated in chapter 10, which is already available in R. We basically only need a few demographic variables out of this data set.
library(simFrame)
data(eusilcP, package = "simFrame")
pop <- eusilcP[, c("age","gender","hsize","hid")]

#In our population, no information is given about fertility. So we will construct it, ignoring the case of single mothers. We will also add a variable that indicates whether one is being partnered or single. 
pop$nchildWomen <- ifelse(pop$gender == "female" & as.integer(pop$hsize) > 2 & pop$age > 17, as.integer(pop$hsize) - 2, 0)
pop$partnered <- factor(ifelse(as.integer(pop$hsize) >= 2 & pop$age > 17, "P", "A"))

#the following line displays the first six observations of our population at time T_0:
head(pop)
#We now can easily define the state space: Interesting so this graphic indicates the state space. I really need to look into dynamic systems research. 

stateSpace <- expand.grid(sex = levels(pop$gender), partnered = levels(pop$partnered))

#Our simplified state space is as the following:
stateSpace

#We are shown what a transition matrix might look like:
trMatrix_f <- cbind(c("female/A->female/P", "female/P->female/A"), c("rates1","rates2"))
trMatrix_m <- cbind(c("male/A-male/P", "male/P-male/A"),c("rates3","rates4"))
allTransitions <- rbind(trMatrix_f, trMatrix_m)
absTransitions <- rbind(c("female/dead","mortRate"), c("male/dead", "mortRate"))

#We use a function from the MicSim package to build the transition matrix: Will need to see what they mean by transition matrix. I really still have a long way to go. 
library(MicSim)
transitionMatrix <- buildTransitionMatrix(allTransitions = allTransitions, absTransitions = absTransitions, stateSpace = stateSpace)

#After manual correction of the transition matrix (rates3 and rate4 were not considered by building transitionMatrix), the transition matrix looks as follows:
transitionMatrix[1,3] <- "rates3"
transitionMatrix[3,1] <- "rates4"
transitionMatrix# really have no clue how he came up with this simulation and object structure. Saddly I still have a very long way to go.

#We need to determine the maximum age of a person, for exmaple:
maxAge <- 100
#to define further transitions is now straightforward, for example, regarding newborns, education level, enrollment to school, and migration. We will skip this but refer to Zinn 2014 for examples. The software tools Modgen and openM++ are probably the most reliable tools. The author's experience with R package MicSim at the time of writing the book is that it does produce errors when any other example data is used, other than the examples given in ?micSim.

##Dynamics of love and hate:
#Dynamic system illustration using Prince Harry's and Chesley Davy's relationship. 
#This derivative expresses that Chelsy Davy's love grows if Prince Harry shows his mischeivous side and her love decreases the more Prince Harry acts nicely to her. The coefficients a and b are positive constants (response coefficients). We will formulate a function called love which we will pass through all parameters.
love <- function(t, x, parms){
	with(as.list(c(parms, x)), {
		dPrince_Harry <- a * Chelsy_Davy
		dChelsy_Davy <- -b * Prince_Harry
		res <- c(dPrince_Harry, dChelsy_Davy)
		list(res)
	})
}

#We then fix the parameters and the length of the love affair. In addition, we assume that at time 0, Prince Harry loves Chelsy Davy but Chelsy Davy does not love him:
parms <- c(a = 1, b = 2)
times <- seq(0,30, length = 31)
##Start values for steady state 
y <- xstart <- c(Prince_Harry = 1, Chelsy_Davy = -1)

#We solve the dynamic system using a general solver for ordinary differential equations:
library(deSolve)
out <- ode(xstart, times, love, parms = parms)

#The following chart shows how Prince Harry's and Chelsy Davy's love develops over time:
matplot(out)# I can't seem to get this function to work properly. Will need to look into what's wrong with it later on through my studies. 

y <- xstart <- c(Prince_Harry = 0.2, Chelsy_Davy = 1)
parms <- c(a = 0.3, b = 0.7)
out <- ode(xstart, times, love, parms = parms)
matplot(out)# Again the values didn't plot the same as the author's representation. Will need to see the internal arguments for this function. 
args(matplot)
??matplot()

##Dynamic systems in ecological modeling:
#The author uses a Toad and snake illustration. Where the toad is the prey in the dynamic system and the snake is the predator. To learn a little more about this example and the rationale behind it look at page 372 in the text.

#We can write the Lotka-Volterra model in this function:
lv_mod <- function(time, state, parms){
	with(as.list(c(state, parms)), {
		dx <- k1 * x - k2 * x * y 
		dy <- -k3 * y + k4 * x * y 
		return(list(c(dx,dy)))
	})
}

#specify the parameters (linear growth of the prey and prey gets eaten by the predator at the rate of 1.5 ...):
parms <- c(k1 = 1, k2 = 1.5, k3 = 0.2, k4 = 0.6)
state <- c(x = 10, y = 10)
time <- seq(0, 200, by = 1)

#We can solve the ordinary differential equations using the ode() function from the deSolve package:
res <- ode(func = lv_mod, y = state, parms = parms, times = time)
rest <- as.data.frame(res)

#We can plot the results using the same matplot() as the earlier data set using the love differential equations.
par(mar = c(4,4, 0.5,0.1))
matplot(res[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Toads","Snakes"),lty = c(1,2), col = c(1,2), box.lwd = 0)

