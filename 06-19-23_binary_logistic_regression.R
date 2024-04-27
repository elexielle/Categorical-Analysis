#LOGISTIC REGRESSION 06-19-23

#BINARY LOGISTIC REGRESSION USING R
#install.packages("ISLR")
library(ISLR)

#step 1: data prep
mydata <- ISLR::Default
View(mydata)
str(mydata)
table(mydata$default, mydata$student)
xtabs(~default+student, data=mydata)

#step 2: train/test split
#install.packages("tidymodels")
library(tidymodels)
split <- initial_split(mydata, prop=0.7)
train <- training(split)
test <- testing(split)

#step 3: fitting the log model
model1 <- glm(default~as.factor(student)+balance+income, 
              family="binomial", data=train) #full model
options(scipen = 999) #to disable scientific notation
summary(model1)

#reduced model
model2 <- glm(default~as.factor(student)+balance, 
              family="binomial", data=train) 
summary(model2)

model3 <- glm(default~as.factor(student)+income, 
              family="binomial", data=train) #w/o balance 
summary(model3)

model4 <- glm(default~+balance+income, 
              family="binomial", data=train) #w/o student 
summary(model4)


## use model 2 and 4 

#feature importance
library(caret)
caret::varImp(model1)

#pseudo r squared: if iv helped
##cox and snell
##nagelkerke
#install.packages("fmsb")
library(fmsb)
NagelkerkeR2(model2)
NagelkerkeR2(model4)

##tjur's: based on likelihood
#install.packages("performance")
library(performance)
compare_performance(model2, model4)

##McFadden
  ###not based on likelihood; 
  ###based on difference between predicted and observed
  ###closer to 1 good fit
#install.packages("pscl")
library(pscl)
pscl::pR2(model2)["McFadden"]
pscl::pR2(model4)["McFadden"]

#step 4: assesing pred ability of the model
#if iv helped reduce error
#aic, bic - predictive ability
#rmse - sse in regre, lower value
compare_performance(model2, model4)

#-2loglikelihood
summary(model2) #residual deviance
summary(model4) 

#hosmer and lemeshow test: goodnest of fit test; for binary responses
#install.packages("glmtoolbox")
library(glmtoolbox)
hltest(model2)
hltest(model4)

#step 5: assumptions
#1. the response variable is binary.
#2. the observations are independent.
#3. the sample size is sufficiently large. 
#4. no multicollinearity among the explanatory variables.
      ###vif not applicable to categorical variables
      ###use spearman (ordinal) / chi square (nominal)
##vif>10, high multicollinearity
#remedial measure: remove IV 
library(car)
car::vif(model2)
car::vif(model4)

#gvif: used for categorical IV

#5. linear relationship between explanatory variables and the logit of the RV.
#box-tidwell test
logbalance <- train$balance*log(train$balance)
logincome <- train$income*log(train$income)
m2 <- glm(default~balance+logbalance, family="binomial", data=train)
summary(m2) #satisfied

m4 <- glm(default~balance+logbalance+income+logincome, family="binomial", data=train)
summary(m4)

#6. no influential values (extreme values or outliers)
#cook's distance>4/n, where n is the total obs.
n <- nrow(train)
4/n #exceed this value == influential observation

outlier2 <- cooks.distance(glm(default~balance, family="binomial",
                               data=train)) #outlier from model 2; 
            ##make another model w/o categorical IV
influential_obs2 <- as.numeric(names(outlier2)[outlier2>(4/n)])
influential_obs2 #influential obs from model 2
train2 <- train[-influential_obs2,] #filter influential obs
nrow(train2)
#go back to step 3 and use train2

#for model 4
outlier4 <- cooks.distance(glm(default~balance+income, family="binomial",
                               data=train))
influential_obs4 <- as.numeric(names(outlier4)[outlier4>(4/n)])
influential_obs4 
train4 <- train[-influential_obs4,]
nrow(train4)


