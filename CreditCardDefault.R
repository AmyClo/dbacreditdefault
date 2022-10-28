setwd("/Users/jenny/Documents/FS/Master/Semester 3/Machine Learning/group work/GroupProjects_Data")
mydata <- read.csv("CreditCardDefault.csv")[,-1]
summary(mydata)
#adjust target var
mydata$default_payment_next_month[which(mydata$default_payment_next_month=="0")] <-"no"
mydata$default_payment_next_month[which(mydata$default_payment_next_month=="1")] <-"yes"
#rename pay_0
library(dplyr)
names(mydata)[names(mydata) == "PAY_0"] <- "PAY_1"
#target var. as factor
mydata$default_payment_next_month <- as.factor(mydata$default_payment_next_month)
#adjust NAs
mydata$MARRIAGE[which(mydata$MARRIAGE=="0")] <- "NA"
mydata$EDUCATION[which(mydata$EDUCATION=="0")] <- "NA"
mydata$EDUCATION[which(mydata$EDUCATION=="5")] <- "NA"
mydata$EDUCATION[which(mydata$EDUCATION=="6")] <- "NA"

mydata$EDUCATION <- as.integer(mydata$EDUCATION)
mydata$MARRIAGE <- as.integer(mydata$MARRIAGE)

is.na(mydata)
colSums(is.na(mydata))
newdata <- na.omit(mydata)
#sampling
dim(newdata)
summary(newdata$default_payment_next_month)
#downsample
library(caret)
downsample <- downSample(newdata[,-24], newdata$default_payment_next_month, yname = "Default_Payment_NXTM" )
summary(downsample$Default_Payment_NXTM)
#upsample
upsample <- upSample(newdata[,-24], newdata$default_payment_next_month, yname = "Default_Payment_NXTM" )
summary(upsample$Default_Payment_NXTM)


summary(downsample)
summary(upsample)

#dummy vars for up sample
upsample$SEX <- as.character(upsample$SEX)
upsample$EDUCATION <- as.character(upsample$EDUCATION)
upsample$MARRIAGE <- as.character(upsample$MARRIAGE)
dummydata_up <- dummyVars("~ .", data = upsample[,2:4])
trsf_up <- data.frame(predict(dummydata_up, newdata = upsample[,2:4]))
trsf_up$SEX2<-NULL
trsf_up$EDUCATION4<-NULL
trsf_up$MARRIAGE3<-NULL

#rearrange df
new_up <- upsample[,5:24]
new_up <- new_up[c(20,1:19)]
new_up[,21] <- upsample[,1]
new_up[,22:27] <- trsf_up

#standarization
library(caret)
preproc_up <- preProcess(new_up[,2:27], method = c ("range"))
trsf_preproc_up <- data.frame(predict(preproc_up, newdata = new_up[,2:27]))
up_preproc <- trsf_preproc_up
up_preproc[,27] <- new_up[,1]

names(up_preproc)[names(up_preproc) == "V27"] <- "Default"
names(up_preproc)[names(up_preproc) == "V21"] <- "LIMIT_BAL"
names(up_preproc)[names(up_preproc) == "SEX1"] <- "Male"
names(up_preproc)[names(up_preproc) == "EDUCATION1"] <- "GraduateSchool"
names(up_preproc)[names(up_preproc) == "EDUCATION2"] <- "University"
names(up_preproc)[names(up_preproc) == "EDUCATION3"] <- "HighSchool"
names(up_preproc)[names(up_preproc) == "MARRIAGE1"] <- "Married"
names(up_preproc)[names(up_preproc) == "MARRIAGE2"] <- "Single"

#partition for upsample data
testidx_up <- createDataPartition(up_preproc$Default, p = 0.2, list = FALSE)
testidx_up
data_train_up <- up_preproc[-testidx_up,]
data_test_up <- up_preproc[testidx_up,]

summary(data_test_up$Default)
summary(data_train_up$Default)

#create report
report <- data.frame(Model=character(), Acc.Train=numeric(), Acc.Test=numeric())

TControl <- trainControl(method="cv", number=10)

#knn
knnGrid_up <- expand.grid(k = c(seq(3, 25, 2)))
knnmodel_up <- train(Default~., data=data_train_up, method="knn", tuneGrid=knnGrid_up, trControl=TControl)
knnmodel_up
prediction.train <- predict(knnmodel_up, data_train_up[,-27], type="raw")
prediction.test <- predict(knnmodel_up, data_test_up[,-27],type="raw")
acctr <- confusionMatrix(prediction.train, data_train_up[,27])
acctr$table
acctr$overall['Accuracy']
accte <- confusionMatrix(prediction.test, data_test_up[,27])
accte$table
accte$overall['Accuracy']
report <- rbind(report, data.frame(Model="k-NN", Acc.Train=acctr$overall['Accuracy'], Acc.Test=accte$overall['Accuracy']))
report

#C5.0
set.seed(123)
c50Grid <- expand.grid(trials = c(1:5),
                       model = c("tree", "rules"),
                       winnow = FALSE)
c5model <- train(Default ~., data=data_train_up, method="C5.0", tuneGrid=c50Grid, trControl=TControl)
c5model 
prediction.trainC5 <- predict(c5model, data_train_up[,-27], type="raw")
prediction.testC5 <- predict(c5model, data_test_up[,-27], type="raw")
acctrC5 <- confusionMatrix(prediction.trainC5, data_train_up[,27])
acctrC5$table
acctrC5$overall['Accuracy']
accteC5 <- confusionMatrix(prediction.testC5, data_test_up[,27])
accteC5$table
accteC5$overall['Accuracy']
report <- rbind(report, data.frame(Model="C5.0", Acc.Train=acctrC5$overall['Accuracy'], Acc.Test=accteC5$overall['Accuracy']))
report

#CART
set.seed(123)
cartmodel <- train(Default ~., data=data_train_up, method="rpart", trControl=TControl)
cartmodel 
prediction.trainCA <- predict(cartmodel, data_train_up[,-27], type="raw")
prediction.testCA <- predict(cartmodel, data_test_up[,-27], type="raw")
acctrCA <- confusionMatrix(prediction.trainCA, data_train_up[,27])
acctrCA$table
acctrCA$overall['Accuracy']
accteCA <- confusionMatrix(prediction.testCA, data_test_up[,27])
accteCA$table
accteCA$overall['Accuracy']
report <- rbind(report, data.frame(Model="CART", Acc.Train=acctrCA$overall['Accuracy'], Acc.Test=accteCA$overall['Accuracy']))
report


#Decision Tree



#Random Forest
library(randomForest)
RFmodel_up <- randomForest(Default ~ ., data = data_train_up)
RF_prediction <- predict(RFmodel_up, data_test_up)
y1 <- data_test_up$Default 
y_pred_v1 <- RF_prediction

precision <- posPredValue(y_pred_v1, y1)
recall <- sensitivity(y_pred_v1, y1)
F1 <- (2*precision*recall) / (precision+recall)

precision #0.9669187 accuracy
recall #0.8895652 proportion of positive results out of samples
F1 #0.9266304 concise the previous metrics and the harmonic average

importance(RFmodel_up,)




