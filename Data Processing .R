# university case study

#import data
data <- read.csv2("university1.csv", stringsAsFactors = TRUE)

#verify data importation
head(data,4)
tail(data,4)
View(data)

#check data structure
str(data)

#recode name in text 
data$Name = as.character(data$Name)
class(data$Name)

# --------------------------
#   verify data outliers
# --------------------------

# qualitative variables

table(data$Higher.degree) #count per category
prop.table(table(data$Higher.degree))  #percentage per category
barplot(table(data$Higher.degree),horiz=F, las=2,
        cov.names = 0.6, col="#EA309D", main = "higher degree")
barplot(table(data$Type),horiz=F, las=0,
        cov.names = 0.8, col="lavender", main = "higher degree")

#group together categories with low frequency
data$Higher.degree.rec[data$Higher.degree=="Bachelor's degree"] <-"Bachelor's degree"
data$Higher.degree.rec[data$Higher.degree=="Doctor's degree other"] <-"Doctor's degree"
data$Higher.degree.rec[data$Higher.degree=="Doctor's degree practice"] <-"Doctor's degree"
data$Higher.degree.rec[data$Higher.degree=="Doctor's degree research"] <-"Doctor's degree"
data$Higher.degree.rec[data$Higher.degree=="Doctor's degree research and practice"] <-"Doctor's degree"
data$Higher.degree.rec[data$Higher.degree=="Master's degree"] <-"Master's degree"


class(data$Higher.degree.rec)

data$Higher.degree.rec <- factor(data$Higher.degree.rec, order=T, levels =c("Bachelor's degree", "Master's degree","Doctor's degree"))

table(data$Higher.degree.rec)
prop.table(table(data$Higher.degree.rec))

#quantitative.variables
hist(data$Total.applicants, main="Total applicants", col="#ED2099")
hist(data$Total.eligibles, main="Total eligibles")
hist(data$Total.qualified, main="Total qualified")
hist(data$Total.students, main="Total students")
hist(data$Tuition.fees, main="Tution fees")

# we observe extreme values for all quantitative variables
# In particular, for total.students and tution.fees, there seems 
# to be one outlier very far from the other values
boxplot(data$Total.applicants, main="Total applicants")
boxplot(data$Total.eligibles, main="Total eligibles")
boxplot(data$Total.qualified, main="Total qualified")
boxplot(data$Total.students, main="Total students")
boxplot(data$Tuition.fees, main="Tution fees")

# manage outliers
# delete observations total students > 400 000 or Tution fees > 150000

data.wo <- data[data$Total.students < 400000 & data$Tuition.fees <150000,]
boxplot(data.wo$Total.students, main="total students", col="#0bd1fc")
boxplot(data.wo$Tuition.fees, main="tuition fees", col="#7d1dd7")

#transform variables
data.wo$log.Total.applicants=log(data.wo$Total.applicants)
data.wo$log.Total.eligibles=log(data.wo$Total.eligibles)
data.wo$log.Total.qualified=log(data.wo$Total.qualified)
data.wo$log.Total.students=log(data.wo$Total.students)
data.wo$log.Tuition.fees=log(data.wo$Tuition.fees)

hist(data.wo$log.Total.applicants, main="Total applicants", col="#ead1dc")
hist(data.wo$log.Total.eligibles, main="Total eligibles")
hist(data.wo$log.Total.qualified , main="Total qualified")
hist(data.wo$log.Total.students, main="Total students")
hist(data.wo$log.Tuition.fees, main="Tution fees")

boxplot(data.wo$log.Total.applicants, main="Total applicants")
boxplot(data.wo$log.Total.eligibles, main="Total eligibles")
boxplot(data.wo$log.Total.qualified , main="Total qualified")
boxplot(data.wo$log.Total.students, main="Total students")
boxplot(data.wo$log.Tuition.fees, main="Tution fees")


#------------------------
# Missing values
# -----------------------

summary(data.wo)

library(mice)
md.pattern(data.wo, rotate.names = T)
# only 10 rows affected by missing values,
# over a total of 1150 rows

#create a dataset without missing values
data.wo.complete <-na.omit(data.wo)
summary(data.wo.complete)
dim(data.wo.complete)

#---------------------
# Normality study
#---------------------

# skewness and kurtosis
# QQ-pLot
# Jarque bera test
# shaquiro test

install.packages("moments")
library("moments")

skewness(data.wo.complete$Total.applicants)
kurtosis(data.wo.complete$Total.applicants)

skewness(data.wo.complete$log.Total.applicants)
kurtosis(data.wo.complete$log.Total.applicants)

#QQ Plot
# when the variable is normally distributed
# the points should be close to the first bisector
qqnorm(data.wo.complete$Total.applicants, main="Normality study total applicants")
qqline(data.wo.complete$Total.applicants)

qqnorm(data.wo.complete$log.Total.applicants, main="Normality study total applicants")
qqline(data.wo.complete$log.Total.applicants)

# Jarque bera test
# ho : the variable is normally distributed
# h1 : the variable is not normally distributed

#pvalue : probability to do wrong by rejecting no
# if p-value < 5%, then we reject h0
# if p-value > 5%, then we accept h0

install.packages('xts')
library(xts)
install.packages('TTR')
library(TTR)
install.packages("tseries")
library(tseries)

jarque.bera.test(data.wo.complete$Total.applicants)
#p-value < <<<5%, we reject normality of the distribution
jarque.bera.test(data.wo.complete$log.Total.applicants)
#p-value = 5.5% we accept Normal approximation

#Shapiro test

shapiro.test(data.wo.complete$Total.applicants)
#p-value < <<<5%, we reject normality of the distribution

shapiro.test(data.wo.complete$log.Total.applicants)
#p-value =0.3% <5%, we still reject normality


# --------------------------------------------------
# Link between type and higher degree (question 6)
# --------------------------------------------------

tab <- table(data.wo.complete$Type, data.wo.complete$Higher.degree.rec)
# row percentage
prop.table(tab, 1)
#column percentage
prop.table(tab,2)

#mosaic plot
mosaicplot(tab,color=hcl(c(360,240,120)), ylab="highest degree", 
                         xlab="type", cex = 0.5, las=1)
#chisquare test
# ho : the two variables are independant
# h1 : the two variables are linked

chisq.test(tab)
#p-value <<<< 5%, so we reject h0 : there is a significant difference between private and public universities

# --------------------------------------------------
# Link between type and tution fees (question 7)
# --------------------------------------------------

tapply(data.wo.complete$Tuition.fees, data.wo.complete$Type, summary)

boxplot(data.wo.complete$Tuition.fees, data.wo.complete$Type, 
        main ="tution fees",
        ylab = "type", xlab = "", las=2, col="pink")

# Two mains comparison test
# h0 : m1=m2
# h1 = m1#m2

t.test(data.wo.complete$Tuition.fees ~ data.wo.complete$Type)

# ---------------------------------------------------------
# Link between higher degree and tution fees (question 7)
# ---------------------------------------------------------

boxplot(data.wo.complete$Tuition.fees ~ data.wo.complete$Higher.degree.rec, col="green")

#anova test 
# h0 : there is a link between tution fees and higher degree
# h1 : there is a libk betweeen the 2 variables

test.aov <- lm(data.wo.complete$Tuition.fees~ data.wo.complete$Higher.degree.rec)
anova(test.aov)

# to compare means 2 by 2 : tukey test
TukeyHSD(aov(test.aov))
