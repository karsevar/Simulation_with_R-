### chapter 2 R and high-performance computing:

## The R statistical environmnent:
## Basics in R:
#R can be used as an overgrown calculator. All the operations of a calculator can be very easily used in R; for example, addition is done with +, subtraction with -, division with /, exponential with exp(), logarithm with log(), square root using sqrt(), sinus with sin(). All operations work as expected. 

5+ 2 * log(3 * 3)# just like in regular mathematical computations. parentheses go first then multiplication, division, addition, subtraction and so on.

##Some very basic stuff about R:
#R is a function and object-oriented language. Functions can be applied to objects. The syntax is as shown in the following example:
mean(rnorm(10))
#with the function rnorm(), 10 numbers are drawn randomly from a standard normal distribution. If no seed is fixed (with function seed()), the numbers differ from one call to another call of these functions. Afterwards, the mean is calculated for these 10 numbers. Functions typically have function arguments that can be set. The syntax for calling a function is generally:
res1 <- name_of_function(v1)
res2 <- name_of_function(v1, v2) 

#Functions often have additional function arguments with default values. You get access to all function arguments with args().
args(data.frame) # cools this command will come in handy if I don't want to read through the documentation of a particular new function.

x <- rnorm(5)
x

#The function options() allows you to modify the default setting such as to change the font, the encoding, or as shown here, we reduce the number of printed digits (interally R will not round to these digits, it's just the print):
options(digits = 4) #this command effectively rounded the values in object x to be rounded up to 4 numeric digits.
x 

#the latest changes are often only available in the development version of the package. Sometimes these development versions are hosted on GitHub or similar Git repository systems. To install the latest development version, the installation of the package devtools is needed. After calling the devtools package, the development version can be installed via install_github(). 
if(!require(devtools)) 
install.packages("devtools")
install_github("hadley/dplyr")

##Help:
#It's crucial to have basic knowledge of how to get help using the following command:
help.start()

#By this command your browser opens and help (and more) is available. The browsable help index of the package can be accessed by typing the following command into R:
help(package = "dplyr")

#To find hep for a specific function, one can use help(name) or ?name. As an example, we can look at the help file of function group_by(), which is included in the package dplyr.
??group_by()#Interest it seems that my initial idea of using double question marks was correct. 

#Data in the package can be loaded via the data() function, for example , the Cars93 dataset from package MASS:
data(Cars93, package = "MASS")

#help.search() can be used to find functions for which you don't know an exact name, for example:
help.search("histogram")
#This command will search your local R installation for functions approximately matching the character string "histograme" in the file name, alias, title, concept, or keyword entries. With function apropos, one can find and list objects by partial name. For example, to list all objects with partial name match of hist, type:
apropos("hist")

#To searc help pages, vignettes or task view, use the search engine at the website of R and view the results of your request in your web browser.
RSiteSearch("group by factor")
#This line of code reports all search results for the character string "group by factor". 

##The R workspace and the working directory:
#The collection of all created objects is called the workspace. To list the objects in the workspace, type the following:
ls()
#When importing or exporting, the working directory must by defined. To show the current working directory, the function getwd can be used (and the commands cd() and setwd() can be used in the R terminal as well to change the working directory to a new or rather different file). 
getwd()
cd("/Users/masonkarsevar/Desktop/rworks")
setwd("/Users/masonkarsevar/Desktop/rworks")# To take the other comment back it seems that the only way to set a working directory is to use the setwd() command. Very interesting. 

## Data types:
#The following are the important data structures:
	#vector
	#list 
	#array
	#data.frame
	#special data types: missing values (NA), MULL-objects, NaN, -inf, +inf.
	
##Vector in R:
v.num <- c(1,3,5.9,7)
v.num

is.numeric(v.num)

#is,numeric query if the vector is or class numeric. Note that characters are written with parentheses.
#Logical vector are often created indirectly from numeric/character vectors:
v.num < 3

#many operations on vector are performed element-wise, for example, logical comparisons ar arithmetic operations with vectors. A common error source is when the length of two or more vectors differs. Then the shorter one is repeated (recycling). 
v1 <- c(1,2,3)
v2 <- c(4,5)
v1 + v2 

#One should also be aware that R coerces internally to meaning ful data types automatically. For example:
v2 <- c (100, TRUE, "A", FALSE)
v2
is.numeric(v2)
#Here, the lowest common data type is a string and therefore all entries of the vector are coerced to character. Note, to create vectors, the functions seq() and rep() are very useful. 

#Often it is necessary to subset vectors. The selection is made using the [] operator. A selection can be done in three ways.
	#positive: A vector of positive integers that specifies the position of the desired elements.
	#Negative: A vector with negative integers indicating the position of the non-required elements.
	#Logical: A logic vector in which the elements are to be selected (true) and not selected (false).
	
	
data(Cars93, package = "MASS")
hp <- Cars93[1:10, "Horsepower"]# Honestly I didn't know that I could do this command very interesting. The base R alternative to this is Tilman Davies' and Hadley Wickham's combined style of using:
library(dplyr)
hp2 <- Cars93 %>% 
	select("Horsepower") %>%
	head(n = 10) %>%
	as.vector()
# Not sure if this is a perfect parallel to this last command but it does leave a lot to the imagination of the many different possibilities with R programming. Very interesting. 

hp3 <- Cars93$Horsepower
hp3 <- head(n = 10, hp3)
#this is a perfect example of an alternative command line. 
hp

hp[c(1,6)]
hp[-c(2:5,7:10)]
hp < 150
hp[hp < 150]
#or rather a better use of this line is to find the names and the statistics of all the cars that display a horsepower value over 150:
Cars93[Cars93$Horsepower < 150,] #or rather this command might be a little more interesting:
Cars93$Make[Cars93$Horsepower < 150]

## Factors in R:
#Factors in R are of special importance. They are used to represent nominal or ordinal data. More precisely, unrecorded factors for nominally scaled data, and ordered factors for ordinal scaled data. Factors can be seen as special vectors. They are internally coded integers from 1 to n (# of occurrences) which are all associated with a name (label). So why should numeric or character variables be used in factors? Basically, factors have to be used for categorical information to get the correct number of degrees of freedom and correct desing matrices in statistical modeling. In addition, the implementation of graphics for factors vs. numerical / character vectors differs. Moreover, factors are more efficient at storing character vectors. However, factors have more complex data structure since factor include a numerically coded data vector and lables for each level/ category. Let us consider the following example:
class(Cars93)
class(Cars93$Cylinders)
levels(Cars93$Cylinders)
summary(Cars93$Cylinders)

#Hadley Wickham's alternative to this line of code:
Cars93 %>%
	group_by(Cylinders) %>%
	count()
	
#We note that output of summary is different for factors. Interally, R applies a method dispatch for generic functions such as summary, searching in our case if the function summary.factor exists. If yes, this function is applied, if not, summary.default is used. 

##List 
# A list in R is an ordered collection of objects whereas each object is part of the list and where the data types of the individual list elements can be different (vectors, matrices, data.frames, lists, and so on). The dimension of each list item can be different. Lists can be used to group and summarize various objects in an object. There are (at least) four ways of accessing elements of a list, 
	#The [] operator, the operator [[]], the operator $ and the name of a list item. With str(), you can view the structure of a list, with names() you get the names of the list elements:
	
model <- lm(Price ~ Cylinders + Type + EngineSize + Origin, data = Cars93)
class(model)# Interesting. I never thought of the objects created with the lm() function as lists befroe this chapter. This is really a cool development. 
model$coefficients# Just like with data.frames and lists you can access the elements within the lm() data structure through the syntax $ name. 

## data.frame
#A data.frame is like a list, where all list elements are vector/factors but with the restriction that all list elements have the same number of elements (equal length) for example, data frame external sources to be read are often stored as data frames, data frames are usually created by reading data but they can be constructed with function data.frame(). 

#A lot of opportunities exist to subset a data frame, for example with sytax: [ index row, index columns]. Again positive, negative and logical indexing is possible and the type of indexing may be different for row indexing and column indexing. Accessing individual columns is easiest using the $ operation (like lists):
#And also don't forget about the dplyr package's subsetting tools like filter() and select() and the subset() function championed by Simon Walkoviak. 
w <- Cars93$Cylinders %in% c("3","4") & Cars93$Horsepower < 80
str(w)
str(Cars93[w,])# Now I understand. the w vector was created as a logical vector subsetting tool that is ultimately used to subset the Cars93 data frame as a means to display all 3 and 4 cylinder cars that display a horsepower value of over 80. 
Cars93[w,]

#A few helpful functions that can be used in conjunction with data frame are: dim(), reporting the dimensions (number of rows and columns); head(), first (default 6) rows of a data frame; colnames(), the columns/variable names. 

##array:
#An array in R can have multiple dimenstions. A vector is already a one-dimensional array. A matrix is a two dimensional array, having rows and columns. Let us call a data set from package vcd stored as a four-dimensional array:
library(vcd)
library(grid)
data(PreSex)
PreSex# Now I understand what the dataset is getting at. The authors ceriously documented their data in an array format in place of a data.frame() or a tibble(). 
??PreSex

#We see that the first dimension is MatrialStatus, the second is ExtramaritalSex, the third dimension is PremaritalSex, and the fourth dimension is Gender.
#We can now access the elements of the array by indexing using []. If we want to extract the data where PremaritalSex is Yes and Gender is Men, we type:

PreSex[,,1,2]
#This means that all values from the first and second dimensions are chosen, only the first one (Yes) from the third and the second one (Men) from the last dimension is specified. This can also be done by name:
PreSex[,, "Yes", "Men"]# this command displays the same data as the latter command, thus meaning that the array() function can array names in place of array dimension values. 

##Missing values:
#Missing values are almost always present in the data. The default representation of a missing value in R is the symbol NA. A very useful function to check if data values are missing is is.na(). It returns a logical vector or data.frame depending on whether the input is a vector or data.frame indicating "missingness". to calculate the number of missing values, we could sum the TRUE's (interpreted as 1 while False is interpreted as 0). 
sum(is.na(Cars93))

#All in all, 13 values are missing.

#To analyze the structure of any missing values, the R package VIM can be used. One out of many possible plots for missing values, the matrixplot shows all the values of the whole data frame. Interestingly, the higher the weight of the cars, the more missings are present in variable luggage.room:
??require
library(VIM)
matrixplot(Cars93, sortby = "Weight", cex.axis = 0.6)# interesting function but the main problem is that I don't know what the corresponding colors represent in this plot. will need to look into this.
??matrixplot()
#In package robCompositions, one useful function is missPatterns, which shows the structure of missing values.
m <- robCompositions::missPatterns(Cars93)

matrixplot(Cars93, sortby = "Weight", cex.axis = 0.6)

##Generic functions, methods, and classes:
#r has different class systems, the most important ones are s3 and s4 classes. Programming with s3 classes is easy living, it's easier than s4. However, s4 is clean and the use of s4 make packages very user-friendly. 

#In any case, in R each object is assigned to a class (the attribute class). Classes allow object-oriented programming and overloading of generic functions. Generic functions produce different output for objects of different classes as soon as methods are written for such classes. 

#As an example of a generic function, we will use the function summary, summary is a generic function used to produce result summaries. The function invokes particular methods that depend on the class of the first argument:
length(methods(summary))
class(Cars93$Cylinders)
summary(Cars93$Cylinders)
summary(as.character(Cars93$Cylinders))

#From the previous example one can see that the summary is different, depending on the class of the object. R internally looks as if a method is implemented for the given class of the object. If yes, this function is used, it not, the function summary.default is used. This procedure is called method dispatch. 

## Data manipulation in R:
#(interesting addition) In any case, when working, for example, with arrays of dimensions larger than 2 apply is still the only choice. One example is the phonological data, where for each phonological station, measurements for different species are given over time. Such data sets are typically stored as an array, in this case in a four-dimensional array. 

##Apply and friends with basic R:
#Using the apply family one can manipulate slices of data from matrices, arrays, lists, and data frames in a repetitive manner. These functions allow you to cross the data over certain dimensions and avoid the explicit use of for loops. Repetitively, functions are applied over all elements of a dimension. 
#Let's consider again the Cars93 data set. The data set consists of rows (the first dimension) and columns (the second dimension). To apply a function over the second dimension of this data set, for example, to calculate the number of missing values over the columns, the call looks like the following. 
func <- function(x){
	return(sum(is.na(x)))
}

na <- apply(X = Cars93, MARGIN = 2, FUN = func)#It seems that the margin argument is used to set the dimension that the apply will focus on. 
na# Interesting most of the na values are located in the Luggage.room column of the dataset Cars93 will need to look into why this is.
na[na > 0]
#Here X must be an array (note that a matrix is a 2-dimensional array), Margin is the dimension where fun is applied. 

#For loop illustration of the apply command above:
p <- ncol(Cars93) 
na_for <- numeric(p) 
for(i in 1:p) {
	na_for[i] <- func(Cars93[,i])
}
identical(as.numeric(na), na_for)

#While one can always use for loops, the code from apply is much shorter.
#When a given function should be applied on a list (note that a data.frame is also internally treated as a list), lapply might be your friend. The output returned is alos a list that has the same number of elements as the object passed to it. 

#In a previous code call, we assigned a list output to an object called m:
m <- robCompositions::missPatterns(Cars93)
class(m)
#We apply a function, for example, length(), to all elements of a list by using lapply() to access the length of each list element:
lapply(m, length)# I wonder if a normal apply() call will works with this data structure. Will need to look into this idea:
apply(X = m, FUN = length)# It seems like the author was right. You should always use lapply() for list data structures.

#sapply works basically as lapply, but simplifies the output if possible. For example:
s <- sapply(m, length)
class(s)

#The function aggregate is similar to apply. Its difference lies in the function argument, which allows it to subset the data set and apply a function on these subsets. Let us have a look at the function arguments:
args(aggregate)
#Since we see only the arguments from the generic, but we want to apply the functions to the Cars93 data frame, we may look to see if there is a method for data frames implemented:
methods(aggregate)
args(aggregate.data.frame)

#To make group-wise statistics, this function can now be applied on our example data, for example, to calculate the median Horsepower and Weight of cars for each cylinder class (Cylinders):
aggregate(Cars93[,c("Horsepower","Weight")], by = list(Cars93$Cylinders), median)# Pretty cools this is the base R equivalent of mutate in the dplyr data science package. 

#Similar functions to aggregate are by (another print output), summarize from package Hmisc, and summarize and group_by from package dplyr.

##Basic data manipulation with the dplyr package:
#The package dplyr offer functions for:
	#Filtering of observations
	#Variable selection
	#Recoding 
	#Grouping 
	#Aggregation (in groups)
	#data.table() 
	
#Some of the steps in data management can be abstracted. Such tasks include: selection of rows or columns -- ordering of data -- recoding, grouping, and aggregation.

#Here are some further reasons for an additional package such as dplyr:
	#Only a few important key words to remember.
	#consistancy 
	#works with different inputs
	#data.frame, data.tables, sqlite
	#Simple (but new) syntax
	#Less code, less error 
	#From now on in this section the following applies (since this is the dplyr language): a column corresponds to a variable and a row corresponds to an observation.
	
library("dplyr")

##dplyr -- creating a local data frame:
#A local data frame can be created using tbl_df().
#Why do we need this? Because it offers more efficient print outputs and no chance of accidentally printing huge data sets, which can lead to memory problems or long waiting time:
class(Cars93)

#We then convert to a local data frame for dplyr and look at the new print output that is done by dplyr:
Cars93 <- tbl_df(Cars93)# neat this converts the Cars93 data frame into a tibble. 
class(Cars93)
print(Cars93)

##dplyr -- selecting lines:
#Using the function slice(), one can select rows according to their line number:
slice(Cars93, 1)# In this case what the author means by line is the first row of the Cars93 dataset. 
# You can also select multiple rows at once.

#Note that c() creates a vector from the input numbers, and function n() returns the number of observations (lines). We will select the 1,4,10,15, and the last line of the data:
slice(Cars93, c(1,4,10,15,n()))# Interesting the n() function can be used as a marker for the last observation of any dataset. Much like the length(Cars93$Cylinder) command line. Interesting it seems that the function n() can only be called within a function call. Will need to look into this later on through my studies. 

#The function filter() can select rows that satisfy a condition.
#Example, all observations where variable Manufacturer == is Audi when at the same time the value of variable Min.Price is > 25:
filter(Cars93, Manufacturer == "Audi" & Min.Price > 25)

##dplyr -- order:
#With arrange() you can sort the data by one or more variables. By default it is sorted in ascending order, with desc() descending:
Cars93 <- arrange(Cars93, Price)
Cars93

#You can sort by multiple variables:
head(arrange(Cars93, desc(MPG.city), Max.Price), 7)

##dplyr -- selecting columns:
#Function select() allows you to select variables from the data set:
head(select(Cars93, Manufacturer, Price), 3)# Now I understand. The final argument (illustrated by 3) in the function call is actually the n argument (which is used to disclose to the console how many rows that need to be printed). 

# For a sequence of variables, the operator: can be used:
head(select(Cars93, Manufacturer:Price), 3)

#Negative indexing is possible, while all variables with the letter prefix minus (-) are excluded:
select(Cars93, -Min.Price, -Max.Price) #This command suppresses the min.price and the max.price variables in the dataset. 

#Some functions are useful within select():
	#starts_with()
	#ends_with()
	#contains()
	#matches()
	#num_range()
	
head(select(Cars93, starts_with("Man")), 3)
head(select(Cars93, contains("Price")), 3)

#both select() and rename() can be used to rename variables by simply using a new = old syntax. select() returns only the specified variables:
head(select(Cars93, myPrice = Price, Min.Price))# Interesting this is very much like the recode() function with Simon Walkowiak's book. 

##dplyr -- uniqueness:
#Functionality distinct() can be used to keep only unique rows:
Cars93_1 <- select(Cars93, Manufacturer, EngineSize)
dim(Cars93_1)
Cars93_1 <- distinct(Cars93_1)
dim(Cars93_1)
??distinct()

#By default, all variables are used to assess whether a row multiple occurs in the data set:
dim(Cars93)
dim(distinct(Cars93, Manufacturer))#Interesting I recieved a very different output (32 rows and 1 column) will need to see why this is the case.

dim(distinct(Cars93, Manufacturer, EngineSize))
dim(distinct(Cars93, Manufacturer, rr = round(EngineSize)))

##dplyr -- creating variables:
#With function mutate() one can add new variables and retains the old variables:
m <- mutate(Cars93, is_ford = Manufacturer == "Ford")
m[1:3, c(1,28)]

#Function tranmutate() retains only the listed variables, in this case it looks almost the same as the previous example:
transmute(Cars93, is_ford = Manufacturer == "Ford", Manufacturer)

#Newly created variables can be used again in the same statement:
head(transmute(Cars93, Manufacturer, is_ford = Manufacturer == "Ford", num_ford = ifelse(is_ford, -1, 1)), 3)

##dplyr -- grouping and aggregates:
#One often wants to perform calculations in groups. Previously, we saw examples using the apply family of the base R package. For data frames, the dplyr package supports grouping. dplyr supports grouping with the function group_by(), which creates the subsets, and summarize(), which is used to calculate the statistics that must provide exactly one number. Package dplyr provides additional, useful aggregation statistics such as the first_value(x), last_value(x), and nth_value(x) of a variable.

by_type <- group_by(Cars93, Type)
summarize(by_type, 
	count = n(), min_es = min(EngineSize), 
	max_es = max(EngineSize)
)

#Via group_by() functions are applied on defined groups. Note that dplyr supports the pipeline syntax from R package magrittr. 
#(important addition) arrange() and select() are independent of grouping.

#Let's take another example, in order to report the first two observations per group:
by_type <- group_by(Cars93, Type)
slice(by_type, 1:2)

#Example using the pipe:
library(tidyverse)
Cars93 %>% group_by(Type) %>% slice(1:2)# Now I understand; the author is ordering the cars according to type and ultimately splitting the Cars93 dataset into only displaying the first two cars of each type in the dataset. Very interesting. 

# Let's take another example. We want to complete a new variable EngineSize at the square of EngineSize, and for each group we want to compute the minimum of the new variable. In addition, the results should be sorted in descending order:
Cars93 %>% mutate(ES2 = EngineSize^2) %>% group_by(Type) %>% summarize(min.ES2 = min(ES2)) %>% arrange(desc(min.ES2))

## dplyr -- window functions:
#summarize() works for functions that return one single value. To make more complex aggregations, window functions can be used.

#There are different types of window functions:
	#Ranking/ordering: row_number(), min_rank(), percent_rank(), and so on.
	#Offsets: lag(), lead()
	#Cumulative functions: cumsum(), cummin(), cummax(), cummean(), and so on.
	
Cars93 %>%
	group_by(Type) %>%
	arrange(Type) %>%
	select(Manufacturer:Price) %>%
	mutate(cmean = cummean(Price), csum = cumsum(Price))
	
## Data manipulation with the data.table package:
#The package data table is not included in the base R installation and must be installed once. It allows very efficient aggregation of large data sets (for example, data with several gigabytes of memory), efficient merging (join) of several objects, adding and deletion of variables, and efficient importing of data sets (fread())

#Let us first convert a data.frame to a data.table using funciton data.table. We again use the Cars93 data, and print the data table -- the print output differs from base R and also from dplyr. Note that each data.table is alos a data.frame and both can be accessed as a list.
require(data.table)
Cars93 <- data.table(Cars93)
Cars93

#the utility function tables() lists all data.table objects in the memory and gives information on the dimension and needed memory for each data table.
tables()

##data.table -- variable construction:
#using the $ operator, new variables can be constructed. As an example, we will create a new variable where the values are TRUE if the manufacturer is Ford.
Cars93$tmpl <- Cars93[, j = Manufacturer == "Ford"]
#We can modify a variable by the :=-syntax directly (a very nice feature).
Cars93[, tmp2 := rnorm(nrow(Cars93))]

#Note that these modifications are done by-reference -- no copy of the data is needed internally.
#To delete variables, one of the two following possibilities can be used:
Cars93[, tmpl:=NULL]
Cars93$tmp2 <- NULL

##data.table -- Indexing or subsetting:
#The indexing is done differently than in base R. Two parameters are used, i : for the rows of the data.table() and j: for the columns of the data.table. 

#We use [] as an indexing operator, but it works slightly differently. j is an expression in the scope of the actual object. Using with = FALSE: j is evaluated as a vector of names or numbers. 

#Let us extract rows. 
Cars93[i = 2]#This is used to extract the second row of all columns.
Cars93[i = c(1,5)]#This is used to extract the first and fifth row of all columns.
Cars93[i = -c(1:5)]#this excludes the first five rows of the Cars93 dataset.

#Now let us extract columns. 
Cars93[j = 3]# Interesting this extracts the type column. Very interesting since the author said that this command should of resulted in an error of some kind.
Cars93[j = "Price"]# Again just like the latter command. This command returned the price column which is curious since the author states that the syntax does not allow "" symbols over column names. Very interesting.

Cars93[j = Price]#Interesting. This command extracts the price column values as a vector. Perhaps the author meant that the other commands were a failure because they only extracted the values as a matrix with 93 rows and 1 column.

Cars93[i = 1:3, j = "Price", with = FALSE]# Now I understand through the command of with = FALSe you're telling the console that the values in the j argument need to be evaluated as a vector. 

#With = FALSE experiment:
Cars93[j = "Price", with = FALSE]#Interesting with FALSE didn't work in this case perhaps you need an i argument for the with argument to work properly.

#Indexing can also be done more sophisticatedly. For example, if we wanted to extract the first three rows, extract all variables, calculate a new variable that is the price range, or calculate the mean price, we could do the following:
Cars93[1:3, .(Price, Horsepower, Diff.Price = Max.Price - Min.Price, Mean.Price = mean(Price))]# In addition according to the author the syntax .() means list.

## data.table -- keys:
#data.table objects can be grouped according to a key. Based on such a key, calculations are very efficient. By using setkey(), a key can be set for a data.table:
setkey(Cars93, Type) # Equally: setkeyv(dt, "x")

#More than one key can be defined for a data.table. Now sorting is done automatically regarding this key. Actual key variables can be displayed with key():
key(Cars93)

##data.table -- fast subsetting:
#By using keys, we can increase the performance of subsetting:
setkey(Cars93, Type)
Cars93["Van"]# so this is very much a variable equivalent of the pipe with the package dplyr. This line automatically knew that I wanted to see the subset Cars93[Type == "Van"].

#For more than two keys --- for example, to extract all observations with Type equal to Van, DriveTrain equals 4WD and Origin equals non-USA -- we can apply the following:
setkey(Cars93, Type, DriveTrain, Origin)
Cars93[c("Van","4WD","non-USA")]# this tool really does save time with dealing with subsetting arguments.

#Let's compare efficiency on a data set with characters. We use the microbenchmark package for this purpose. We see that data.table is more than 60 times faster than base R, and in this case dplyr is the slowest.
library(microbenchmark)
require(microbenchmark)
library(tidyverse)
N <- 1000000
dat <- data.table(
	x = sample(LETTERS[1:20], N, replace = TRUE),
	y = sample(letters[1:5], N, replace = TRUE))
# Neat very good to know letters and LETTERS are both alphabet datasets. Really cool. 
	
head(dat, 3)

setkey(dat, x, y)

microbenchmark(
	data.table = dat[list(c("B","D"), c("b","d"))],
	dplyr = dat %>% slice(x %in% c("B","D") & y %in% c("b","d")),
	baseR = dat[x %in% c("B","D") & y %in% c("b","d")]
)
# The author is right the data.table() function is way faster. Very interesting.

## data.table -- calculations in groups:
#We can do calculations in groups by using by. In the following example, we will calculate the arithmetic mean price, the interquartile price range, and the median price:
Cars93[, .(mean = mean(Price), IQR = IQR(Price), median = median(Price)), by = Type]# very cool this command is just like the dplyr:
Cars93 %>%
	group_by(Type) %>%
	summarize(mean = mean(Price), median = median(Price, IQR = IQR(Price)))
# Neat these two methods resulted in the same values.

#There is further functionality in the data.table package. See .SD to apply functions to more than one variable, .N for the number of elements in each group, and merge to efficiently join data sets.

##High performance computing:
#Initially, it is important to measure which lines of code take the most computation time. Here, you should try to solve problems with the processing time of individual calculations by improving the computation time. This can often be done in R by vectorization, or often better by writing individual pieces of code in a compilable language. In addition, some calculations can be parallelized and accelerated through parallel computing.

##Profiling to detect computationally slow functions in code:
#Take an example where you have written code for your data analysis but it runs (too) slow. However, it is most likely that not all your lines of code are slow and only a few lines need improvement in terms of computational time. In this instance it is very important to know exactly what step in the code takes the most computation time. 

#the easiest way to find this out is to work with R function system.time. 

data(Cars93, package = "MASS")
set.seed(123)
system.time(lm(Price ~ Horsepower + Weight + Type + Origin, data = Cars93))
library(robustbase)
system.time(lmrob(Price ~ Horsepower + Weight + Type + Origin, data = Cars93))

#the user time is the CPU time for the call and evaluation of the code. The elapsed time is the sum of the user time and the system time. this is the most interesting number. proc.time is another simple function, often used inside functions:
ptm <- proc.time()
lmrob(Price ~ Horsepower + Weight + Type + Origin, data = Cars93)

robustbase::lmrob(formula = Price ~ Horsepower + Weight + Type + Origin, data = Cars93)
proc.time() - ptm

#to get a more precise answer about the computational speed of the methods, we should replicate the experiment. We can see that lm is about 10 times faster than lmrob:
s1 <- system.time(replicate(100, lm(Price ~ Horsepower + Weight + Type + Origin, data = Cars93)))[3]
s2 <- system.time(replicate(100, lmrob(Price ~ Horsepower + Weight + Type + Origin, data = Cars93)))[3]
(s2 - s1) / s1

#However, we don't know which part of the code makes a function slow:
Rprof("Prestige.lm.out")
invisible(replicate(100, 
				lm(Price~ Horsepower + Weight + Type + Origin, data = Cars93)))
Rprof(NULL)
summaryRprof("Prestige.lm.out")$by.self

#We can see which function calls relate to the slowest part of the code:
# A more detailed output is reported by using the following. 
library(profr)
require(profr)
parse_rprof("Prestige.lm.out")

#Further benchmarking:
#Finally, we will show a data manipulation example using several different packages. this should show the efficiency of data.table and dplyr.

library(microbenchmark)
library(dplyr)
library(plyr)
library(data.table)
library(Hmisc)

#the task is to calculate the groupwise (type, origin) means of horsepower for example:
data(Cars93, package = "MASS")
Cars93 %>% group_by(Type, Origin) %>% summarise(mean = mean(Horsepower))

#First, we calculate the same with base R, where we also write a for loop for calculating the mean. We do this extra dirty for benchmarking purposes:
meanFor <- function(x){
	sum <- 0 
	for(i in 1:length(x)) sum <- sum + x[i]
	sum / length(x)
}

##groupwise statistics:
myfun1 <- function(x, gr1, gr2, num){
	x[,gr1] <- as.factor(x[,gr1])
	x[,gr2] <- as.factor(x[,gr2])
	11 <- length(levels(x[,gr1]))
	12 <- length(levels(x[,gr1]))
	gr <- numeric(11*12)
	c1 <- c2 <- character(11*12)
	ii <- jj <- 0
	for(i in levels(x[,gr1])){
		for(j in levels(x[,gr2])){
			ii <- ii + 1 
			c1[ii] <- i 
			c2[ii] <- j 
			vec <- x[x[,gr2] == j & x[,gr1] == i, num] 
			if(length(vec) > 0) gr[ii] <- meanFor(vec)
		}
	}
	df <- data.frame(cbind(c1,c2))
	df <- cbind(df, gr)
	colnames(df) <- c(gr1, gr2, paste("mean(", num, ")"))
	df
}
## groupwise using mean() 
##attention mean.default is faster 
myfun2 <- function(x, gr1, gr2, num){
	x[,gr1] <- as.factor(x[,gr1])
	x[,gr2] <- as.factor(x[,gr2])
	11 <- length(levels(x[,gr1]))
	12 <- length(levels(x[,gr1]))
	gr <- numeric(11*12)
	c1 <- c2 <- character(11*12)
	ii <- jj <- 0
	for(i in levels(x[,gr1])){
		for(j in levels(x[,gr2])){
			ii <- ii + 1 
			c1[ii] <- i 
			c2[ii] <- j 
			gr[ii] <- mean(x[x[,gr2] == j & x[,gr1] == i, num])
		}
	}
	df <- data.frame(cbind(c1,c2))
	df <- cbind(df, gr)
	colnames(df) <- c(gr1, gr2, paste("mean(", num, ")"))
	df
}

#For data.table, we will create a data table:
library(data.table)
Cars93dt <- data.table(Cars93)
setkey(Cars93, Type, Origin)
key(Cars93)
Cars93[, mean(Horsepower), by = .(Type, Origin)]# Important addition to keep in mind is that unlike with base R the arguments within the data.table package only accepts lists in place of vectors. Hence the .() syntax. 

#We now run the benchmark using microbenchmark and plot the results. For the plot syntax, refer to the section on visualization:
op <- microbenchmark(
	MYFUN1 = myfun1(x = Cars93, gr1 = "Type", gr2 = "Origin", num = "Horsepower"),
	MYFUN2 = myfun2(x = Cars93, gr1 = "Type", gr2 = "Origin", num = "Horsepower"),
	PLYR = ddply(Cars93, .(Type, Origin), summarise, output = mean(Horsepower)),
	AGGR = aggregate(Horsepower ~ Type + Origin, Cars93, mean),
	BY = by(Cars93dt$Horsepower, list(Cars93$Type, Cars93$Origin), mean),
	SUMMARIZE = summarize(Cars93$Horsepower, llist(Cars93$Type, Cars93$Origin), mean),                                                                      
	TAPPLY = tapply(Cars93$Horsepower, interaction(Cars93$Type, Cars93$Origin), mean),
	DPLYR = summarise(group_by(Cars93, Type, Origin), mean(Horsepower)),
	DATATABLE = Cars93dt[, aggGroup1.2 :=mean(Horsepower), by = .(Type, Origin)],
	times = 1000L)
	
#It seems that the functions and for loop examples aren't working properly. The error message is: 
#Error in 11 <- length(levels(x[, gr1])) : 
  #invalid (do_set) left-hand side to assignment
  
 op <- microbenchmark(
	PLYR = ddply(Cars93, .(Type, Origin), summarise, output = mean(Horsepower)),
	AGGR = aggregate(Horsepower ~ Type + Origin, Cars93, mean),
	BY = by(Cars93$Horsepower, list(Cars93$Type, Cars93$Origin), mean),
	SUMMARIZE = summarize(Cars93$Horsepower, llist(Cars93$Type, Cars93$Origin), mean),                                                                      
	TAPPLY = tapply(Cars93$Horsepower, interaction(Cars93$Type, Cars93$Origin), mean),
	DPLYR = summarise(group_by(Cars93, Type, Origin), mean(Horsepower)),
	DATATABLE = Cars93dt[, aggGroup1.2 :=mean(Horsepower), by = .(Type, Origin)],
	times = 1000L)
	
#Again, I seemed to have ran into a problem with the myfun1 and myfun2 functions (or rather for loops). But with that said, the main problems with the other command calls was that Cars93 was converted into a data.table during some far away conversion command. So in other words, all the other commands are working just fine (I believe). 
m <- reshape2::melt(op, id = "expr")
ggplot(m, aes(x = expr, y = value)) + 
	geom_boxplot() +
	coord_trans(y = "log10") + 
	xlab(NULL) + ylab("computation time") +
	theme(axis.text.x = element_text(angle = 45))
	
#Cool I finally got the boxplot to print on my console. This looks somewhat like the values the author obtained . 
#We can see that both dplyr and data.table are no faster than the others. Even the dirty for loops are almost as fast. 
#But we get a very different picture with large data sets:
library(laeken)
data(eusilc)
eusilc <- do.call(rbind, 
	list(eusilc, eusilc, eusilc, eusilc, eusilc))
eusilc <- do.call( rbind, 
	list(eusilc, eusilc, eusilc, eusilc, eusilc))
dim(eusilc)

eusilcdt <- data.table(eusilc)
setkeyv(eusilcdt, c('hsize', 'db040'))

op <- microbenchmark(
	PLYR = ddply(eusilc, .(hsize, db040), summarise, output = mean(eqIncome)),
	AGGR = aggregate(eqIncome ~ hsize + db040, eusilcdt, mean),
	BY = by(eusilc$eqIncome, list(eusilc$hsize, eusilc$db040), mean),
	SUMMARIZE = summarize(eusilc$eqIncome, llist(eusilc$hsize, eusilc$db040), mean),                                                                      
	TAPPLY = tapply(eusilc$eqIncome, interaction(eusilc$hsize, eusilc$db040), mean),
	DPLYR = summarise(group_by(eusilc, hsize, db040), mean(eqIncome)),
	DATATABLE = eusilcdt[, mean(eqIncome), by = .(hsize, db040)],
	times = 1000L)
	
#Again we plot the results, as shown in the following device:
m <- reshape2::melt(op, id = "expr")
ggplot(m, aes(x = expr, y = value)) + 
	geom_boxplot() +
	coord_trans(y = "log10") + 
	xlab(NULL) + ylab("computation time") +
	theme(axis.text.x = element_text(angle = 45))
#Will need to run this dataset again later on through my studies. My computer is saddly not up to the task of running these large dataset without a parallel storage and computing platform. 

#We can now observe that data.table and dylr are must faster than the other methods (the grapics shows log-scale!).

#We can further profile the functions, for example, for aggredate we see that calling gsub and prettyNum needs the most computation time in function aggregate:
Rprof("aggr.out")
a <- aggregate(eqIncome ~ hsize + db040, eusilc, mean)
Rprof(NULL)
summaryRprof("aggr.out")$by.self# Interesting. It seems that I'm getting different output variables with this function than the author. will need to look into way there is a discrepency. 

##Parallel computing:
#Parallel computing is helpful, especially for simulation tasks, since most simulations can be done independently. Several functions and packages are available in R. For this example, we will show only the simple package snow. Using Linux and osx, one can use the package parallel, for foreach works for all platforms.

#Again the Cars93 data is used. We want to fit the correlation coefficient between Price and Horsepower using a robust method (minimum covariance determinant - MCD), and in addition we want to fit the corresponding confidence interval by a Bootstrap. Basically, we take Bootstrap samples (using sample()) and for each Bootstrap sample we calculate the robust convariance. From the results (R values), we take certain quantiles which can then by used for determining the confidence interval:
R <- 10000
library(robustbase)
covMcd(Cars93[,c("Price","Horsepower")], cor = TRUE)$cor[1,2]

##confidence interval:
n <- nrow(Cars93)
f <- function(R, ...) {
	replicate(R, covMcd(Cars93[sample(1:n, replace = TRUE),
		c("Price", "Horsepower")], cor = TRUE)$cor[1,2])
}
system.time(ci <- f(R))# Really my computer is super sluggish with these computations. This is really hilarious. I could have sworn that it took the computer about 5 minutes to complete just this calculation. 

#The aim now is to parallelize this calculation. We will call the package snow and make three clusters. Note that you can make more clusters if you have more CPUs on your machine. You shoud use the number of CPUs you have minus one as the maximum.
library(snow)
cl <- makeCluster(3, type = "SOCK")

#We now need to make the package robustbase available for all nodes as well as the data and the function:
clusterEvalQ(cl, library("robustbase"))
clusterEvalQ(cl, data(Cars93, package = "MASS"))
clusterExport(cl, "f")
clusterExport(cl, "n")
clusterSetupRNG(cl, seed = 123)
#It seems that I might have to forgo this exercise since I don't have a parallel computing rig complete with over two different CPUs and I can't seem to load RNGstream onto my R terminal. 

#With clusterCall we can perform parallel computing:
system.time(ci_boot <- 
	clusterCall(cl, f, R = round(R / 3)))
quantile(unlist(ci_boot), c(0.025, 0.975))
#We see that we are approximately twice as fast, and we could be even faster if we had more CPUs available. 
#Finally, we want to stop the cluster. 
stopCluster(cl)

#Found out that the package this calculate was looking for was "rlecuyer". Hence through running the command:
library(rlecuyer) #this line of code works perfectly. The author was right this is way faster than the other applications. Will need to test this out on other calculations that were out of my reach just a while ago. 

##Interfaces to C++:
#Interfaces to C++ are recommended in order to make certain loops faster. We will show a very simple example to calculate the weighted mean. it should highlight the possibilities and let readers first get into touch with the package Rcpp, which simplifies the use of C++ code dramatically compared with R's .Call function.

#We want to compare the runtime of an example using R as an interpreted language, and also using Rcpp. We want to calculate the weighted mean of a vector.

#A naive r function could look like that. We will use only interpreted R code:
wmeanR <- function(x, w){
	total <-0
	total_w <- 0
	for( i in seq_along(x)){
		total <- total + x[i] * w[i]
		total_w <- total_w + w[i]
	}
	total / total_w
}

#there is also a function called weighted.mean available in the base installation of R, and weightedMean in package laeken.

#Let us also define the Rcpp function. The function cppFunction compiles and links a shared library, then internally defines an R function that uses .Call:
library(Rcpp)
cppFunction('
	double wmean(NumericVector x, NumericVector w) {
		int n = x.size();
		double total = 0, total_w = 0;
		for(int i = 0; i < n; ++i){
			total += x[i] * w[i];
			total_w += w[i]; 
		}
		return total / total_w;
	}
')

#Now, let's compare the methods:
x <- rnorm(100000000)
w <- rnorm(100000000)
library(laeken)
op <- microbenchmark(
	naiveR = wmean(x, w),
	weighted.mean = weighted.mean(x, w),
	weightedMean = weightedMean(x, w),
	Rcpp.wmean = wmean(x, w),
	times = 1
)# Interesting. It seems like naive R is faster on my machine than the author's computer.

m <- reshape2::melt(op, id = "expr")
ggplot(m, aes(x = expr, y = value)) +
	geom_boxplot() + coord_trans(y = "log10") + xlab(NULL) + ylab("computation time") + theme(axis.text.x =element_text(angle = 45, vjust = 1))# funny weighted.mean and weihtedMean were by far the worst with regards to computational time. It seems that naive R has been very much optimized since they last updated the R console. Very cool!
	
##Visualizing information:
#In many chapters, results are visualized using the graphics capabilities of R. thus, we will give a very short introduction to the base graphics package, plus a short introduction to the package ggplot2.
#The reader will learn briefly about the graphical system in R, different output formats for traditional graphics system, customization and fine tuning of standard graphics, and the ggplot2 package. 

#Other packages such as ggmap, ggvis, lattice, and grid are not touched on here. 

##The graphics system in R:
# Many packages include methods to produce plots. Generally, they either use the functionality of the base R package called graphics or the functionality of the package grid. For example, the package maptools includes methods for mapping, with this package one can produce maps. it uses the capabilities of the graphics package. Package ggplot2 is based on the grid package and uses the functionality of the grid package for constructing advanced graphics. 

#In practice, there is almost no difference between plotting on the screen or in a PDF. Note that the current state of a device can be stored and copied to other devices. The most common graphic devices are X11(), pdf(), postscript(), png(), and jpg(). 

#Ideas for study: self taught programmer (java), code academy, code school, plural site

#For example:
getwd()
pdf(file ="myplot.pdf")
plot(Horsepower~ Weight, data = Cars93)
dev.off()

#The available graphics devices are machine dependent. You can check the available ones using :
?Devices

#Note that various function arguments such as width, height, and quality can be used to modify the output.

##the graphics package:
#The package graphics is the traditional graphics system. Even though there are other packages for visualization available that produce quality plots perfectly, the graphics package is mainly used for producing figures quickly in order to explore content on the fly. Within this package different types of functions exist:
	#High-level graphics functions: opens a device and create a plot (example plot() and most likely ggplot())
	#Low-level graphics functions: Add output to existing graphics (for example, points())
	#interactive functions: allow interaction with graphical output (identify())
	
#Typically, a combination of multiple graphics functions is used to create a plot.
#Each graphics device can be seen as a sheet of paper. Thus with the graphics package we can draw using many pens in many colors, but no eraser is available. Multiple devices can be simultaneously opened and one can draw in one graphics device.

##Warm up example -- a high-level plot:
#We want to plot filled circles where the size depends on the absolute value of y and the color depends on the value of x:
x <- 1:20 / 2
y <- sin(x)
plot(x, y, pch = 16, cex = 10 * abs(y), col = grey(x / 14))

#Let's add some text, a curve and a line:
plot(x, y, pch = 16, cex = 10 * abs(y), col = grey(x / 14))
text(x, y, 1:20, col = "yellow")
curve(sin, -2 * pi, 4 * pi, add = TRUE, col= "red")
abline(h = 0, lty = 2, col = "grey")

#the function plot() is a generic function. It is overloaded with methods, and R selects the method depending on the class of the object to be plotted (method dispatch of R). It also shows different outputs depending on the class of the object to be plotted. 

#This can be seen in the following, where a numeric vector is first plotted, and then a factor, and then a data frame with on variable as a factor, and finally an object of class ts.
par(mfrow = c(2,2))
mpg <- mtcars$mpg
cyl <- factor(mtcars$cyl)
df <- data.frame(x1 = cyl, x2 = mpg)
tmpg <- ts(mpg)
plot(mpg); plot(cyl); plot(df); plot(tmpg)# Cool all the graphics are printed on one device. Will need to remember the ; syntax.

#to know which plot methods are currently available, one can type methods(plot).
tail(methods(plot))
length(methods(plot))

#Subsequent calls produce equivalent results. Note that the last call uses the formula interface of R:
plot(x = mtcars$mpg, y = mtcars$hp)
plot(mtcars$mpg, mtcars$hp)
plot(hp ~ mpg, data = mtcars)

##Control of graphics parameters:
#Graphical parameters are the key to changing the appearance of graphics, including, for example, colors, fonts, linetypes, and axis definitions.
#Each open device has its own independent list of graphics parameters and most parameters can be directly specified in high or low level plotting functions. Important: all graphic parameters can be set via function par. The most important function argument of par are mfrow for multiple graphics, col for colors, lwd for line widths, cex for the size of symbols, pch for selecting symbols, and lty for different kinds of lines. 

#Here are some possibilities: in R you can default address colors by name via colors(). rgb() to mix red-green-blue. Using hsv() is even better as it offers a predefined set of pallets with rainbow and many others ?rainbow and palette().

#Prefefined palettes using the RColorBrewer package.

#Multiple plots with package graphics can be done via par(mfrow = c(2,2)). However, a more flexible graphic can be created through the use of the function layout.

#We will show one example for layout by constructing a plot that doesn't exist in R. 

#This creates both a min and max axis:
xmin <- min(mtcars$mpg); xmax <- max(mtcars$mpg)
ymin <- min(mtcars$hp); ymax <- max(mtcars$hp)
#calculate histograms:
xhist <- hist(mtcars$mpg, breaks = 15, plot = FALSE)
yhist <- hist(mtcars$hp, breaks = 15, plot = FALSE)
#maximum count:
top <- max(c(hist$counts, yhist$counts))
xrange <- c(xmin, xmax)
yrange <- c(ymin, ymax)

#We new ploduce the following figure:
m <- matrix(c(2, 0, 1, 3), 2, 2, byrow = TRUE)
#define plot order and size:
layout(m, c(3,1), c(1,3), TRUE)
#First plot:
par(mar = c(0,0,1,1))
plot(mtcars[,c("mpg","hp")], xlim = xrange, ylim = yrange, xlab = "", ylab = "")
#second plot -- barchart of margin:
par(mar = c(0,0,1,1))
barplot(xhist$counts, axes = FALSE, space = 0)
#third plot -- barchart of other margin:
par(mar= c(3,0,1,1))
barplot(yhist$counts, axes = FALSE, space = 0, horiz = TRUE)
# Cool representation but I believe that the author messed up with the margins and when I attempted to run this command according to his specifications the console returned an error saying that it did not recognize the top in the xlim and ylim argument calls. Will need to look into where the problem is. With that said, this command works just fine without the xlim and ylim arguments.

##The ggplot2 package:
#In ggplot2, the parts of a plot are defined independently. The anatomy of a plot consists of: data, must be a data frame (object of class data.frame (or even tibble)); and aesthetic mapping, which dexcribes how variables in the data are mapped to visual properties of geometric objects. This must be done within the function aes(). assignment, where values are mapped to visual properties. It must also be done outside a function aes(). geometric objects (geom's, aesthetic will be mapped to geometric objects), for example, geom_point() - statistical transformations, for example, function stat_boxplot(), scales, coordinate system, position adjustments, and faceting, for example, function facet_wrap. 

#Aesthetic means "something you can see", for example, colors, fill (color), shape (of points), linetypes, size, and so on. Aesthetic mapping to geometric objects is done using function aes().

#to make a scatterplot with Horsepower and MPG.city for the Cars93 data set, you use the following command:
library("ggplot2")
ggplot(Cars93, aes(x = Horsepower, y = MPG.city)) + geom_point(aes(color = Cylinders))

#Note that each type of geom accepts only a subset of aesthetics (for example, setting shape in aes() makes no sense when mapping to geom_bar()).

#We can simply use more than one geom, here also add a scatterplot smoothing, and we perform an aesthetic mapping of weights to color (within aes()).
ggplot(Cars93, aes(x = Horsepower, y = MPG.city)) + 
	geom_point(aes(color = Weight)) +
	geom_smooth()
#geom_smooth: method = "auto" and the size of largest group is <1000, so the loess method is used. use the argument method to change the smoothing method. 

ggplot(Cars93, aes(x = Horsepower, y = MPG.city)) + 
	geom_text(aes(label = substr(Manufacturer, 1,3)), size = 3.5) +
	geom_smooth()
	
#A standardized graphic for each group in the data can be done -- for one grouping variable use facet_wrap(), for two grouping variables use facet_grid(). 

ggplot(Cars93, aes(x = Horsepower, y = MPG.city)) + 
	geom_point(aes(color = Price, shape = Origin)) +
	facet_wrap(~ Cylinders) + theme_bw()
	
#Note that themes can be used for cooperative design. Two themes comes with ggplot2:theme_gray() (the default) and theme_bw().

