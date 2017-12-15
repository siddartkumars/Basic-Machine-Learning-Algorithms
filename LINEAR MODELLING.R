# Name: Siddarth Kumar S
# Su id : 541866744


library(readr)
library(readxl)
library(data.table)
library(RCurl)

#Read in data from the following URL: 
require(RCurl)
require(gdata)
url <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"
downloaded_data <- read.xls(url)
colnames(downloaded_data) <- c("Fawn_count","Population_adultantelop","annual_preciptation","bad_winter")
View(downloaded_data)
#3.	You should inspect the data using the str() command to make sure that all of the cases have been read in (n=8 years of observations) and that there are four variables.  
str(downloaded_data)

#4.	Create bivariate plots of number of baby fawns versus adult antelope population, the precipitation that year, and the severity of the winter. Your code should produce three separate plots. Make sure the Y-axis and X-axis are labeled. Keeping in mind that the number of fawns is the outcome (or dependent) variable, which axis should it go on in your plots?
#plot 1
try <- lm(Fawn_count~Population_adultantelop,downloaded_data)
lm(formula = downloaded_data$Population_adultantelop ~ downloaded_data$Fawn_count)
summary(try)
plot(try)
summary(try)$r.squared==cor(downloaded_data$Fawn_count,downloaded_data$Population_adultantelop)^2
summary(try)$r.squared ==cor(downloaded_data$Fawn_count,downloaded_data$Population_adultantelop)^2
plot(downloaded_data$Fawn_count~downloaded_data$Population_adultantelop)
abline(try)


#plot 2

try2 <- lm(downloaded_data$Fawn_count~downloaded_data$annual_preciptation)

lm(formula = downloaded_data$annual_preciptation~downloaded_data$Fawn_count)
summary(try2)
plot(try2)

summary(try2)$r.squared ==cor(downloaded_data$Fawn_count,downloaded_data$annual_preciptation)^2
summary(try2)$r.squared ==cor(downloaded_data$Fawn_count,downloaded_data$annual_preciptation)^2


plot(downloaded_data$Fawn_count~downloaded_data$annual_preciptation)
abline(try2)



#plot 3

try3 <- lm(downloaded_data$Fawn_count~downloaded_data$bad_winter)
lm(formula = downloaded_data$bad_winter ~ downloaded_data$annual_preciptation)
summary(try3)
plot(try3)

summary(try3)$r.squared ==cor(downloaded_data$Fawn_count,downloaded_data$bad_winter)^2
summary(try3)$r.squared ==cor(downloaded_data$Fawn_count,downloaded_data$bad_winter)^2

plot(downloaded_data$Fawn_count~downloaded_data$bad_winter)
abline(try3)


# Linear Regression prediction 

  
  set.seed(123)
  split <- sample(seq_len(nrow(downloaded_data)), size = floor(0.75 * nrow(downloaded_data)))
  trainData <- downloaded_data[split, ]
  testData <- downloaded_data[-split, ]
  head(trainData)
  
  linear <- lm( formula = Fawn_count ~ bad_winter + Population_adultantelop, data=trainData)
  summary(linear)
  #plot(downloaded_data$Fawn_count ~ downloaded_data$bad_winter + downloaded_data$Population_adultantelop)
  
  prediction <- predict(linear, testData , type = "response")
  prediction
  head(testData$Fawn_count)
  
  
  SSE <- sum((testData$Fawn_count - prediction) ^ 2)
  SST <- sum((testData$Fawn_count - mean(testData$Fawn_count)) ^ 2)
  1 - SSE/SST

#########################################################################################
  
#5. Next, create three regression models of increasing complexity using lm(). In the first model, predict the number of fawns from the severity of the winter. In the second model, predict the number of fawns from two variables (one should be the severity of the winter). In the third model predict the number of fawns from the three other variables. Which model works best? Which of the predictors are statistically significant in each model? If you wanted to create the most parsimonious model (i.e., the one that did the best job with the fewest predictors), what would it contain?
fit1 = lm(Fawn_count~bad_winter,data=downloaded_data)
summary(fit1)
plot(fit1)

#Inference of Model 1 : Winter variable is significant (Not overly significant)

## Model 2
fit2 = lm(Fawn_count~bad_winter+Population_adultantelop,data=downloaded_data)
summary(fit2)
plot(fit2)

#Inference of Model 2 : Winter variable is not significant but population is significant (Again not overly significant)

## Model 3
fit3 = lm(Fawn_count~bad_winter+Population_adultantelop+annual_preciptation,data=downloaded_data)
summary(fit3)
plot(fit3)


#Inference of Model 3: All 3 variables turns out to be significant (Again not overly significant)

## Anova test is used to find the best linear model amongst nested models

## Carrying out Anova test

anova(fit1,fit2)
anova(fit2,fit3)

## Model 3 is the best since it has the least RSS Value and has the highest R-Squared value