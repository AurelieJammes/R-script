# Training exercise for missing values

install.packages("mlbench")
library("mlbench")

# initialize the data
data("BostonHousing", package ="mlbench")
original <- BostonHousing # backup original data
dataset <-  BostonHousing # data with missing values

str(dataset)
summary(dataset)

#introduce missing values in the dataset
set.seed(100) # to set the seed for the random number generator
dataset[sample(1:nrow(dataset),40), "rad"] <- NA
dataset[sample(1:nrow(dataset),40), "ptratio"] <- NA

View(dataset)

#check for the number of complete cases 
sum(complete.cases(dataset))

# check for number of missing values
sum(is.na(dataset))

# pattern of missing values
install.packages("mice")
library(mice)
md.pattern(dataset, rotate.names=TRUE) #pattern of missing values

# how to deal with missing values 

#1- delete the observations
cleandata <- dataset[complete.cases(dataset),]
dim(cleandata)

#example using na.omit
lm(medv~ptratio+rad, data=dataset,na.action=na.omit)

#linear regression with Y=medv ; x1=ptratio; x2=rad

#2- delete the variables
#if one variable contains all the missing values and these missing values 
# are affecting most of the row, then you should delete the variable

# 3- impute mean or median or mode, or a fixed value
dataimp <-dataset # data with imputation
summary(dataset) # descriptive statistics
sd(dataset$ptratio, na.rm = T) #standard deviation

# when comparing standard deviation wrt mean , we see that an average, there is 
# 2/18 =10% of variation around the average value
# so the dispersion of the variable is low and we could replace the missing value
# either by mean or median

# we prefer the median (compared to the mean) when there are outliers in the dataset

dataimp$ptratio[is.na(dataset$ptratio)] <- median(dataset$ptratio, na.rm = T)
# na.rm=T means to remove all NA values for the computation of the median 

# compute to accuracy when missing values is replaced by median 
actual <- original$ptratio[is.na(dataset$ptratio)]
predicted <- rep(median(dataset$ptratio, na.rm =T), length(actual))
error  <- actual-predicted

#RMSE : riit mean square erro
sqrt(mean(error^2))

#MAE : mean absolute error
mean(abs(error))

#NAPE : root mean square erro
mean(abs(error/actual))

#Both RMSE and MAPE values can only be used as a benchmark to compare
#with the accurancy of another method

#MAPE has an easier interpretation : it shows on average the percentage of deviation
#with respect to the true value

# 4 - more sophisticated imputation methods using nice package
imp <- mice(dataset) # function used to impute missing value
dataset_imputed <- complete(imp)
# complete datset where missing values have been replaced by imputed value
actual <- original$ptratio[is.na(dataset$ptratio)]
predicted <- dataset_imputed[is.na(dataset$ptratio), "ptratio"]
error2  <- actual-predicted

#RMSE : riit mean square error
sqrt(mean(error2^2))

#MAE : mean absolute error
mean(abs(error2))

#NAPE : root mean square error
mean(abs(error2/actual))

# The gain is using more sophisticated methods is not very large here : 8% deviation
# instead of 10%
