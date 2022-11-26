# ---------------------------------------------
# Exercise on linear regression and extension 
#----------------------------------------------

# package with datasets
install.packages("ISLR2")
library("ISLR2")

?Boston
data <- Boston

# -------------------------
# output variabke Y = medv
# -------------------------

# ---------------
# Data structure 
#  ---------------
str(data)
data$chas <- as.factor(data$chas)
class(data$chas)
View(data)

# check the variables rad
summary(data$rad)
#Issue : should we consider it as quantitative or as a factor...
#If the categories are ordered, the, considering it as quantitative is not a too bad idea

unique(data$rad)

#----------------------------
#Outliers and missing values
#----------------------------

#Missing values
summary(data)
#Outliers for medv

hist(data$medv)
boxplot(data$medv)

#heavy tail on the right hand side of the distribution
data$logmedv <- log(data$medv)
par(mfrow = c(2,1))

hist(data$medv)
hist(data$logmedv)
par(mfrow = c(1,1))

par(mfrow = c(2,1))
boxplot(data$medv)
boxplot(data$logmedv)
par(mfrow = c(1,1)) 

#With the log transformation, we observe heavy tail, in the left hand side
#the distribution seems still more centered around the mean...

#------------------------
#simple regression model
#------------------------

#Link between logmedv and lstat
#------------------------------

#Graph

attach(data) #avoid reference to the datatset
plot(logmedv~lstat)

#Model estimation 
lm.fit1 <- lm(logmedv~lstat) #logmedv = b_0 +b_1 * lstat
summary(lm.fit1)

#Interpretation
# Both pvalues are very small (<5%)), so both coefficients are statistically significant (#from 0)
# the coefficient associated with lstat is negative:
# an increase of lstat will have on average a negative impact
# The model explains 65% of prices variations (adjusted r^2)
# (explanatory power of model)

plot(lstat, logmedv, pch="+")
abline(lm.fit1, col="red")

# ------------------
#Residuals analysis 
#------------------

#Normality of residuals
#QQ Plot
qqnorm(residuals(lm.fit1))
qqline(residuals(lm.fit1), col="red")

# we observe that some extreme points do not fit on the line 
# except from these extreme values, the residuals distribution
# is not too far from normality ...

#shape of residuals
plot(predict(lm.fit1), residuals(lm.fit1))
plot(predict(lm.fit1), rstudent(lm.fit1))
abline(a=2,b=0,col="red")
abline(a=-2, b=0, col="red")
# rstudent : standardized residuals (residuals/stdev)
# for normally distributed variables standardized values
# should lie between -2 and 2 (with 95% of chance)

#---------------------------------------
# Evaluation of the quality of the model
#---------------------------------------
# Predictive power using validation set
#---------------------------------------

#sampling the dataset
set.seed(1)
row.number <-sample(1:nrow(data), 0.8*nrow(data))
train=data[row.number,]
test=data[row.number,]
dim(train)
dim(test)

#etimate the model on the training set
lm.fit0.8 <- lm(logmedv~lstat, data=train)

#compute prediction error, RMSE, MAPE
pred0.8 <- predict(lm.fit0.8, newdata=test)
err0.8 <- pred0.8-test$logmedv
rmse <- sqrt(mean(err0.8^2))
mape <- mean(abs(err0.8/test$logmedv))

c(RMSE = rmse, MAPE = mape, R2 = summary(lm.fit0.8)$r.squared) #to print the 3 parameters

plot(test$logmedv, pred0.8, col = "blue")
abline(a=0, b=1, col = "red")

#LOOCV method
#-------------

install.packages("boot")
library("boot")
glm.fit <- glm(logmedv~lstat, data=data)
cv.error <- cv.glm(data, glm.fit)
cv.error$delta[1] # to print th croo-validation statistics

#The best model is the one with the lowest CV statistics

#multiple regression model
#-------------------------
View(data)
str(data)
# correlations between explanatory variables 2 by 2
cor(data[,-4]) #to remove the qualitative `
# difficult to interpret because too many coefficients

#VIF
install.packages("car")
library(car)

vif(lm(logmedv~,medv,data=data))

#, means we use all variables as predictors 
#medv means we exlude medv variable

# there is a multicolinearity issue if VIF > 10
# for this regression, no multicolinearity issue, we can keep all the explanatory varibales 

#check for multicolinearity


# correlations between explanatory variables 2 by 2
# VIF

#variable selection
# -----------------
