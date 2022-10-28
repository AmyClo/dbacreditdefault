setwd("~/Desktop/ML/group project")

CreditCardDefault<- read.csv("CreditCardDefault.csv")[,-1]
#check for missing values
is.na(CreditCardDefault)
colSums(is.na(CreditCardDefault))

#rename pay_0
library(dplyr)
names(CreditCardDefault)[names(CreditCardDefault) == "PAY_0"] <- "PAY_1"
#target var. as factor
CreditCardDefault$default_payment_next_month <- as.factor(CreditCardDefault$default_payment_next_month)

#check outlier
hist(CreditCardDefault$SEX)
hist(CreditCardDefault$EDUCATION)
hist(CreditCardDefault$AGE)
hist(CreditCardDefault$MARRIAGE)

#adjust outliers
CreditCardDefault$MARRIAGE[which(CreditCardDefault$MARRIAGE=="0")] <- "NA"
CreditCardDefault$EDUCATION[which(CreditCardDefault$EDUCATION=="0")] <- "NA"
CreditCardDefault$EDUCATION[which(CreditCardDefault$EDUCATION=="5")] <- "NA"
CreditCardDefault$EDUCATION[which(CreditCardDefault$EDUCATION=="6")] <- "NA"

CreditCardDefault$EDUCATION <- as.integer(CreditCardDefault$EDUCATION)
CreditCardDefault$MARRIAGE <- as.integer(CreditCardDefault$MARRIAGE)

colSums(is.na(CreditCardDefault))
newdata <- na.omit(CreditCardDefault)

#sampling
dim(newdata)
summary(newdata$default_payment_next_month)

#downsample
library(caret)
downsample <- downSample(newdata[,-24], newdata$default_payment_next_month, yname = "Default_Payment_NXTM" )
summary(downsample$Default_Payment_NXTM)

downsample<-downsample[,c(2:23,1,24)]

#preprocess (standalize the numerical number)
preProcValues <- preProcess(downsample[,4:23], method = c("range") )
preProcValues
scaleddata<-predict(preProcValues, newdata=downsample[,4:23])
#pca的话是再搞什么啊一堆pc



#dummy
#turn number into character
downsample$SEX <- as.character(downsample$SEX)
downsample$EDUCATION <- as.character(downsample$EDUCATION)
downsample$MARRIAGE <- as.character(downsample$MARRIAGE)
dmy <- dummyVars(" ~. ", data = downsample[,1:3],fullRank = T)
trsf <- data.frame(predict(dmy, newdata = downsample[,1:3]))
print(trsf)

scaleddata[,21:26]<-trsf
#target为毛变成v27了啊！需要再设一遍target吗
scaleddata[,27]<-downsample[,24]
names(scaleddata)[names(scaleddata) == "V27"] <- "Default_NXTM"
names(scaleddata)[names(scaleddata) == "SEX2"] <- "Female"

#pca


#partition
testidx_down <- createDataPartition(scaleddata$Default_NXTM, p = 0.2, list = FALSE)
testidx_down
data_train_down <- scaleddata[-testidx_down,]
data_test_down <- scaleddata[testidx_down,]
summary(data_train_down)
summary(data_test_down)

#creat report?
report <- data.frame(Model=character(), Acc.Train=numeric(), Acc.Test=numeric())

#C5.0
library(C50)
c5model<-C5.0(Default_NXTM ~., data = data_train_down)
summary(c5model)
plot(c5model)
prediction.train<-predict(c5model, type = "class", data_train_down[,-27])
prediction.test<-predict(c5model,type = "class", data_test_down[,-27])
acctr<-confusionMatrix(prediction.train,data_train_down[,27])
acctr$table
acctr$overall['Accuracy']

#accuracy 0.7251
accte <- confusionMatrix(prediction.test, data_test_down[,27])
accte$table
accte$overall['Accuracy']
#accuracy 0.7079

#cart
library(rpart)
cartmodel <- rpart(Default_NXTM ~., method="class", data=data_train_down)
cartmodel
plot(cartmodel)
text(cartmodel)

prediction.train <- predict(cartmodel,type="class",data_train_down[,-27]) 
prediction.test <- predict(cartmodel,type="class",data_test_down[,-27])
acctr <- confusionMatrix(prediction.train, data_train_down[,27])
acctr$table
acctr$overall['Accuracy']
#0.70

accte <- confusionMatrix(prediction.test, data_test_down[,27])
accte$table
accte$overall['Accuracy']
#accuracy 0.69

