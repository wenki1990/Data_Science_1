setwd("F:\\Library\\Analytics Path\\R\\R DataSet\\Linea Reg")

insurance = read.csv("insurance.csv")
head(insurance)

### pre checks 
str(insurance)
summary(insurance)
### target variable should follow normal distribution 
View(insurance)
hist(insurance$charges)

### target variable is right skewed 
## apply transformation 

insurance$log_charges = log( insurance$charges)

hist(insurance$log_charges)


#hist(sqrt(insurance$charges))

### linear relationship b/w input and target 

plot(insurance$age, insurance$log_charges)
plot(insurance$age , insurance$charges)

### correlation b/w input and target variable

cor( insurance$age, insurance$log_charges) ## Correlation with target variable 


### multicollinearity ( input variables are correlated with each other)
cor(insurance[,c(1,3,4)])
cor( insurance$age, insurance$bmi) ### cor with input variable ( not desirable)
 
## Imapct of smokers on insurance premiums
library(ggplot2)
ggplot( insurance, aes( smoker, log_charges)) + geom_boxplot()

### MOdel bulding  

insurance$charges = NULL 

## train and test set split 

set.seed(675)

ids = sample( nrow(insurance), nrow(insurance)*0.8)

train = insurance[ids,]
test = insurance[-ids,]

## model 

lin_model = lm(log_charges ~ . , data=train)

summary(lin_model)

## Test the model 

test$pred = predict(lin_model, newdata=test )

summary(lin_model)

### RMSE 

test$error = test$log_charges - test$pred

test$error_sq = test$error ** 2

rmse = sqrt(mean(test$error_sq))
rmse

summary(test$log_charges)
(0.43/9.178)*100


###### Diagnosis #####################
### select only a few variable 
fit = lm(log_charges  ~  . , data=train)

plot(fit)
#### correlation check or Multicollinearity 
summary(fit)
names(fit)

fit$coefficients

head(fit$residuals)

### check for normality of errors 
hist(fit$residuals)

### check for auto correlation and heteroscedasticity
  library(MASS) ## need to calculate standardised residuals

residuals = stdres(fit) ## Studentised residuals 
summary(residuals)
plot(residuals)

### predicted values vs. residuals 

plot(fit$fitted.values, residuals)

### statistical test for autocorelation
library(car)
durbinWatsonTest(fit)  ## to check auto correlation

##### Outliers test
library(car)
outlierTest(fit)

### leverage statistics ( cooks.distance)

cd = cooks.distance(fit)
cutoff = 4/(nrow(train))
 
 ### plot for finding the obs which has high leverage(using cd) 
plot(fit, which=4, cook.levels=cutoff)

#Outlier and levergae observations
 
431, 220, 1028, 1040, 103,527, 1020, 
### Variance inflation factor to check multicollinearity 
vif(fit)

 
 ## rebuild the model by removing outlier observations 
 
train = train[ -c( 431, 220, 1028, 1040, 103,527, 1020), ]
 
 
 ## fit the model 
 fit = lm(log_charges  ~. -bmi , data=train)
 
 summary(fit)
 
 ## check if Autcorrelation or Heteroscedasticty has improved 
 plot(fit, which=3)
 
 ##  explore the relationship b/w target and input 
 
 ggplot( train, aes( bmi, log_charges)) + geom_point()
 
 ggplot( train, aes( age, log_charges)) + geom_point()
 
### cobine bmi and age into a new variable 
 
 train$age_bmi = log(train$bmi/train$age)

 ggplot( train, aes( age_bmi, log_charges)) + geom_point()
 
#### final model with ratio of age and bmi 
 
fit = lm( log_charges ~ . -bmi -age,  data=train)
summary(fit) 

plot(fit)

train$bmi.age = log( train$bmi*train$age)

ggplot( train, aes(sqrt(bmi.age), log_charges)) + geom_point()


fit = lm( log_charges ~ . -bmi -age -age_bmi, data=train)

summary(fit)

plot(fit, which = 3)

