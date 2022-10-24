#import data
setwd("C:/Users/amily/Desktop/H2 2022-2023 MiM/05_Machine Learning for Big Data/Group Project")
Rawdata <- read.csv("CreditCardDefault.csv")
summary(Rawdata)
str(Rawdata)

#check na data
colSums(is.na(Rawdata))
#education = unknown removed
newdata= Rawdata[Rawdata$EDUCATION<5,]
str(newdata)
# detect 0 data -> remain 29669 obs.
colSums(newdata==0)
# delete 0 data -> remain 29601 obs.
newdata$EDUCATION[which(newdata$EDUCATION=="0")] <- NA
newdata$MARRIAGE[which(newdata$MARRIAGE =="0")] <- NA
newdata <- na.omit(newdata)
summary(newdata)
str(newdata)
#detect outlier
boxplot(scale(newdata[,2:7]))

#convert target variable from numeric to categorial
newdata$default_payment_next_month=as.factor(newdata$default_payment_next_month)
summary(newdata)

#change variable [3:5] [7:12]to categorial, 
newdata$SEX=as.factor(newdata$SEX)
newdata$EDUCATION=as.factor(newdata$EDUCATION)
newdata$MARRIAGE=as.factor(newdata$MARRIAGE)
summary(newdata)

library(caret)
#exclude target variable
TransData=newdata[,-25]
dmy <- dummyVars(" ~ .", data = TransData, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = TransData))
str(trsf)
summary(trsf)

#include target variable
trsf$default_pay=newdata$default_payment_next_month
#exclude ID
trsf$ID=NULL

# write.csv(trsf, file="C:/Users/amily/Desktop/H2 2022-2023 MiM/05_Machine Learning for Big Data/Group Project/trsf.csv", append=FALSE, col.names=TRUE, row.names= FALSE)

#downsample -> 6605:6605
set.seed(123)
SampleData <- downSample(trsf[, -ncol(trsf)], trsf$default_pay, yname = "default_pay")
summary(SampleData$default_pay)
dim(SampleData)

# normalize
preProcValues <- preProcess(SampleData, method = "range")
NormalizedData <- predict(preProcValues, SampleData)
summary(NormalizedData)

# partition into 75% training and 25% test samples
set.seed(123)
index <- createDataPartition(NormalizedData$default_pay, p=0.75, list=FALSE)
training <- NormalizedData[index,]
test <- NormalizedData[-index,]

summary(training$default_pay)
summary(test$default_pay)

# Classification

#**********************
# k-Nearest Neighbors
#**********************

#randomForrest
#logisticRegression
#decisionTree
