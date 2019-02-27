
rm(list=ls())
#Pre-Processing
traindata<- read.csv("C:/IndividualProject/Data/datatraining.txt",sep=",",header = TRUE)
testdata1<- read.csv("C:/IndividualProject/Data/datatest.txt",sep=",",header = TRUE)
testdata2<- read.csv("C:/IndividualProject/Data/datatest2.txt",sep=",",header = TRUE)


#Check if data is clean and doen not contain any null value
sum(is.na(traindata))
sum(is.na(testdata1))
sum(is.na(testdata2))

#Removing Time from Date Column
traindata$date <- (as.Date(traindata$date))
testdata1$date <- as.Date(testdata1$date)
testdata2$date <- as.Date(testdata2$date)
traindata1<-traindata

#Combining the test datasets and forming complete datasets
testdata<- rbind(testdata1,testdata2)
alldata<- rbind(traindata,testdata)

#Check for duplicated rows and remove them
library(dplyr)
alldata<- distinct(alldata)


#Conveting response variable to factors
traindata$Occupancy<- factor(traindata$Occupancy)
testdata1$Occupancy<- factor(testdata1$Occupancy)
testdata2$Occupancy<- factor(testdata2$Occupancy)
alldata$Occupancy<- factor(alldata$Occupancy)
testdata$Occupancy<-factor(testdata$Occupancy)
#EDA
library(ggplot2)
#Occupancy Count versus date
ggplot(alldata, aes(alldata$Humidity, fill =(Occupancy))) +
  geom_histogram(alpha = 0.5) + xlab("Humidity in %") +
  ylab("Count") +
  ggtitle("Occupancy versus Relative Humidity")+theme_bw()

#Occupation Count versus Temperature
ggplot(alldata, aes(alldata$Temperature,fill=Occupancy))+
  xlab("Temperature in Degree Celcius") +
  ylab("Count") +
  ggtitle("Occupancy versus Temperature")+geom_histogram(bins=30)+theme_bw()


#Variable Selection

library(MASS)
#Stepwise AIC
allpred=glm(Occupancy ~., data=traindata1)
summary(allpred)
stepwise = stepAIC(allpred,trace=FALSE)
stepwise$anova


#Modelling
#Logistic Regression
fit1<- train( Occupancy ~ ., data=traindata, method = "glm",family = "binomial",trControl=trainControl(method = "cv", number = 5)) 
predict1<-predict(fit1,testdata)

#Model Validation
confusion.matrix <- table(testdata$Occupancy, predict1)
print(addmargins(confusion.matrix))
accuracy1<- (confusion.matrix[1,1]+confusion.matrix[2,2])/nrow(testdata)
precision1<-(confusion.matrix[2,2])/(confusion.matrix[2,1]+confusion.matrix[2,2])
accuracy1
precision1

#Random Forests
library(randomForest)
fit2 <- randomForest(traindata,traindata$Occupancy)
predict2 <- predict(fit2, testdata)

#Model Validation
confusion.matrix2 <- table(testdata$Occupancy, predict2)
print(addmargins(confusion.matrix2))
accuracy2<- (confusion.matrix2[1,1]+confusion.matrix2[2,2])/nrow(testdata)
precision2<-(confusion.matrix2[2,2])/(confusion.matrix2[2,1]+confusion.matrix2[2,2])
accuracy2
precision2
