library(ggplot2)
library(ggthemes)
library(viridis)
library(lubridate)
library(qdap)
library(tm)
library(ngram)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(tidyverse)

VGData_ver2 <-read.csv('E:/document/coolyah/SEMESTER 6/DAMING/dataset/Video_Games_Sales_as_at_22_Dec_2016.csv', header = TRUE)

#Eksplorasi Data

str(VGData_ver2)
summary(VGData_ver2)

VGData_ver2[VGData_ver2 == "N/A"] = NA
summary(VGData_ver2)

#Missing Value
library("mice")
##Cek missing value
md.pattern(VGData_ver2)

colSums(is.na(VGData_ver2))

boxplot(VGData_ver2$Critic_Count, main="Boxplot Critic_Count", ylab="Critic_Count") 
boxplot(VGData_ver2$Critic_Score, main="Boxplot Critic_Score", ylab="Critic_Score") 
boxplot(VGData_ver2$User_Count, main="Boxplot Critic_Count", ylab="Critic_Count") 

VGData_ver2 <- data.frame(VGData_ver2)
#Praproses

## Critic_Score :int
VGData_ver2$Critic_Score[is.na(VGData_ver2$Critic_Score)]<- median(VGData_ver2$Critic_Score, na.rm = TRUE)
summary(VGData_ver2$Critic_Score)
md.pattern(VGData_ver2)

## Critic_Count :int
VGData_ver2$Critic_Count[is.na(VGData_ver2$Critic_Count)]<- median(VGData_ver2$Critic_Count, na.rm = TRUE)
summary(VGData_ver2$Critic_Count)
md.pattern(VGData_ver2)

## User Count : int
VGData_ver2$User_Count[is.na(VGData_ver2$User_Count)]<- median(VGData_ver2$User_Count, na.rm = TRUE)
summary(VGData_ver2$User_Count)
md.pattern(VGData_ver2)

str(VGData_ver2)

boxplot(VGData_ver2$Critic_Count, main="Boxplot Critic_Count", ylab="Critic_Count") 
boxplot(VGData_ver2$Critic_Score, main="Boxplot Critic_Score", ylab="Critic_Score") 
boxplot(VGData_ver2$User_Count, main="Boxplot Critic_Count", ylab="Critic_Count") 

#Remove NA
data = VGData_ver2
data$Year_of_Release<-as.factor(data$Year_of_Release)

levels(data$Year_of_Release) 
data$Year_of_Release = factor(data$Year_of_Release, levels=c(levels(data$Year_of_Release), "Not Specified"))
data$Year_of_Release[is.na(data$Year_of_Release)] = "Not Specified"

levels(data$Publisher)
data$Publisher = factor(data$Publisher, levels=c(levels(data$Publisher), "Unknown"))
data$Publisher[is.na(data$Publisher)] = "Unknown"
levels(data$Publisher)

#data=na.omit(data)
str(data)
md.pattern(data)

#SVM MODEL
library(caret)
library(e1071)

set.seed(1234)
ind<-sample(2,nrow(data),replace=TRUE,prob = c(0.7,0.3))
traindata<-data[ind==1,]
testdata<-data[ind==2,]

#Correlation of the sales Factor
num_Sales=VGData_ver2[,c("NA_Sales","EU_Sales","JP_Sales","Other_Sales","Global_Sales")]
cor(num_Sales)

## Linear classification
#pemodelan menggunakan data training
y.train=ifelse(traindata$Global_Sales>0.085,"Sales >","Sales <")
dat=data.frame(x=traindata$NA_Sales+traindata$EU_Sales, y=as.factor(y.train))
svmfit=svm(y~., data=dat, kernel="linear", cost=50,scale=FALSE)
print(svmfit)

#hasil pemodelan
table(Model=svmfit$fitted , Truth=dat$y)
cat("Model Error = ", mean(svmfit$fitted!=dat$y)*100,"%")

#prediksi pada data testing
y.test=ifelse(testdata$Global_Sales>0.085,"Sales >","Sales <")
dat.te=data.frame(x=testdata$NA_Sales+testdata$EU_Sales, y=as.factor(y.test))

#hasil prediksi
pred.te=predict(svmfit, newdata=dat.te)
table(Predict=pred.te, Truth=dat.te$y)
cat("Prediction Error = ", mean(pred.te!=dat.te$y)*100,"%")


## Radial classification
#pemodelan menggunakan data training
svmfit=svm(y~., data=dat, kernel="radial", cost=50, gamma=1)
print(svmfit)
#hasil pemodelan
table(Model=svmfit$fitted , Truth=dat$y)
cat("Model Error = ", mean(svmfit$fitted!=dat$y)*100,"%")

#prediksi pada data testing
y.test=ifelse(testdata$Global_Sales>0.085,"Sales >","Sales <")
dat.te=data.frame(x=testdata$NA_Sales+testdata$EU_Sales, y=as.factor(y.test))
pred.te=predict(svmfit, newdata=dat.te)

#hasil prediksi
table(predict=pred.te, truth=dat.te$y)
cat("Prediction Error = ", mean(pred.te!=dat.te$y)*100,"%")


data$Year_of_Release=as.numeric(as.character(data$Year_of_Release))
#Histogram of Global sales of the game by year
aggregate_revenue=aggregate(Global_Sales~Year_of_Release,data,sum)
plot(aggregate_revenue,type='h',xlab="Year_of_Release",ylab="Global Sales",col = "green", lwd = 8, main = "Global Sales per year")

#Histogram of frequency of the game by year
hist(data$Year_of_Release,col = "blue",xlab = "Year of release",ylab = "Frequency of the game", main = "Histogram of frequency of the game by year")
