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
dummydata_up <- dummyVars("~ .", data = upsample[,2:4], fullRank = T)
trsf_up <- data.frame(predict(dummydata_up, newdata = upsample[,2:4]))
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

names(up_preproc)[names(up_preproc) == "V27"] <- "Default_NXTM"
names(up_preproc)[names(up_preproc) == "V21"] <- "LIMIT_BAL"
names(up_preproc)[names(up_preproc) == "SEX2"] <- "Female"


#partition for upsample data
testidx_up <- createDataPartition(up_preproc$Default_NXTM, p = 0.2, list = FALSE)
testidx_up
data_train_up <- up_preproc[-testidx_up,]
data_test_up <- up_preproc[testidx_up,]

summary(data_test_up$Default_NXTM)
summary(data_train_up$Default_NXTM)

#creat report
report <- data.frame(Model=character(), Acc.Train=numeric(), Acc.Test=numeric())

#knn
library(class)
ks <- 3:25
for (k in ks[seq(1, length(ks), 2)])
{
  knnmodel_up <- knn(train=data_train_up[,-27], test=data_test_up[,-27], cl=data_train_up$Default_NXTM, k=k)
  num.correct.labels <- sum(knnmodel_up == data_test_up$Default_NXTM)
  accuracy <- num.correct.labels / length(data_test_up$Default_NXTM)
  cat(k, ", ", accuracy, "\n")
}

summary(knnmodel_up)

