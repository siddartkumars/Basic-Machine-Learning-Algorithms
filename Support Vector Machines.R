##SIDDARTH KUMAR S
### SU ID : 541866744

## Reading the data
s0 = data.frame(airquality)
View(s0)
s0$Ozone[is.na(s0$Ozone)] <- mean(s0$Ozone, na.rm = TRUE)
s0$Solar.R[is.na(s0$Solar.R)] <- mean(s0$Solar.R, na.rm = TRUE)

## Splitting Train and Test
#train = s0[s0$Month!=9,]
#test = s0[s0$Month==9,]

newaq <- s0
randIndex <- sample(1:dim(newaq)[1])
cutPoint2_3 <-floor(2*dim(s0)[1]/3)
train <- s0[randIndex[1:cutPoint2_3],]
test <- s0[randIndex[(cutPoint2_3+1):dim(s0)[1]],]


## Removing missing Ozone records from test (Only 1 was available though)
test = test[!is.na(test$Ozone),]

## Building KSVM model
library(kernlab)
fit = ksvm(Ozone~.,data=train)

## Projecting it on Test Data
test$predictions = predict(fit,test)

## Computing the RMSE value
library(DMwR)
regr.eval(test$Ozone,test$predictions,stats='rmse')

## Chart for the above obtained predictions
test$error = test$Ozone - test$predictions
library(ggplot2)
plot1 = ggplot(test, aes(Temp, Wind, size=error,col=error)) +
  geom_point() + ggtitle("Plot of Temp V/s Wind with Errors of KSVM")
plot1

#### Projections using SVM from (e1071 Package)
library(e1071)
fit1 = svm(Ozone~.,data=train)

## Projecting it on Test Data
test$predictions1 = predict(fit1,test)

## Chart for the above obtained predictions
test$error1 = test$Ozone - test$predictions1
library(ggplot2)

plot2 = ggplot(test, aes(Temp, Wind, size=error1,col=error1)) +
  geom_point() + ggtitle("Plot of Temp V/s Wind with Errors of SVM") 
plot2
#### Projections using LM
fit2 = lm(Ozone~.,data=train)

## Projecting it on Test Data
test$predictions2 = predict(fit2,test)

## Chart for the above obtained predictions
test$error2 = test$Ozone - test$predictions2
library(ggplot2)
plot3 = ggplot(test, aes(Temp, Wind, size=error2,col=error2)) +
  geom_point() + ggtitle("Plot of Temp V/s Wind with Errors of LM")
plot3
## Arranging al 3 plots in the same grid
library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol=2)

########################Step 4: Creating good variable#############################

s0$goodOzone = ifelse(s0$Ozone < mean(s0$Ozone, na.rm=T), 0,1)
s1 <- s0
s1 <- s1[,-1]
View(s1)  
######################STEP 5####################

newaq <- s1
randIndex <- sample(1:dim(newaq)[1])
cutPoint2_3 <-floor(2*dim(s1)[1]/3)
train2 <- s1[randIndex[1:cutPoint2_3],]
test2 <- s1[randIndex[(cutPoint2_3+1):dim(s1)[1]],]

#set.seed(123)
#split <- sample(seq_len(nrow(data_air)), size = floor(0.75 * nrow(data_air)))
#trainData <- data_air[split, ]
#testData <- data_air[-split, ]

## Removing missing Ozone records from test (Only 1 was available though)
fit3 = ksvm(goodOzone~.,data=train2,
            kernel="rbfdot", # kernel function that projects the low dimensional problem into higher dimensional space
            kpar="automatic",# kpar refer to parameters that can be used to control the radial function kernel(rbfdot)
            C=10, # C refers to "Cost of Constrains"
            cross=10, # use 10 fold cross validation in this model
            prob.model=TRUE)

## Projecting it on Test Data
test2$predictions = predict(fit3,test2)
test2$predictions = round(test2$predictions)

## Percent of correct prediciton
perc_ksvm <- length(which(test2$goodOzone== test2$predictions))/dim(test2)[1]
perc_ksvm*100 

## Computing the RMSE value
library(DMwR)
regr.eval(test2$goodOzone,test2$predictions,stats='rmse')

## Chart for the above obtained predictions
test2$error = test2$goodOzone - test2$predictions
library(ggplot2)
plot4 = ggplot(test2, aes(Temp, Wind, size=error,col=goodOzone)) +
  geom_point() + ggtitle("Plot of Temp V/s Wind with Errors of KSVM")
plot4

#### Projections using SVM from (e1071 Package)
library(e1071)
test2 <- s1[randIndex[(cutPoint2_3+1):dim(s1)[1]],]
fit4 = svm(goodOzone~.,data = train2 ,scale = FALSE)
fit4

## Projecting it on Test Data
test2$predictions1 = predict(fit4,test2[,1:6])
test2$predictions1 = round(predictions1) # to increase accuracy

## Percent of correct model
perc_svm <- length(which(test2$goodOzone == test2$predictions1))/dim(test2)[1]
perc_svm*100 

## Chart for the above obtained predictions
test2$error1 = test2$goodOzone - test2$predictions1

##############Scatter Plotting the model##############
library(ggplot2)
plot5 = ggplot(test2, aes(Temp, Wind, size=error1,col=goodOzone))+
  geom_point() + ggtitle("Plot of Temp V/s Wind with Errors of SVM") 
plot5

#### Q4)Projections using NB
library(e1071)
fit5 = naiveBayes(goodOzone~.,data=train2)
fit5

## Projecting it on Test Data
test2$predictions2 <- predict(fit5,test2,type="raw")
test2$predictions2 = round(test2$predictions2)

## Percent of correct predictions
perc_nb <- length(which(test2$goodOzone== test2$predictions2))/dim(test2)[1]
perc_nb*100 

## Chart for the above obtained predictions
test2$error2 = test2$goodOzone-test2$predictions2
test2$error2 <- round(test2$error2)
######### PLotting the model######################
library(ggplot2)
plot6 = ggplot(test2, aes(Temp, Wind)) +
  geom_point(aes(size=error1,color= goodOzone))+ ggtitle("Plot of Temp V/s Wind with Errors of NB")
plot6

##Q5 Arranging al 3 plots in the same grid
library(gridExtra)
grid.arrange(plot4, plot5, plot6, ncol=2)

########### Step 6############################
## I was able to train the model to a 100 % accuracy using Naive Bayes. It is a very powerful model, yet since it is a small dataset,
#I was able to acheive this accuracy,
