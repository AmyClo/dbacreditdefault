# 2	Data Preparation
# 2.1	Handling Missing Values
data = data.frame(x = c(rep(NA,5),11:15), y = c(1:5,rep(NA,5)))
data
is.na(data)
colSums(is.na(data))
newdata <- na.omit(data)
newdata
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}
data

# 2.2	Sampling
# Random Sampling
setwd("D:/Dropbox/Lehre/Digital Analytics/Software/R")
data <- read.csv("churn-data.csv")
head(data)
dim(data)
summary(data$CHURN)
data$CHURN <- as.factor(data$CHURN)  # feature churn has to be converted into a categorical variable
summary(data$CHURN)

index <- sample(1:nrow(data), nrow(data)*0.1, replace=FALSE)
index
datasample <- data[index,]
dim(datasample)
head(datasample)
summary(datasample$CHURN)

# demonstrate 
sample(c(2,5,3), size=3, replace=TRUE)   # repeat multiple
sample(c(2,5,3), size=3, replace=FALSE)

# Proportional Sampling
library(caret)
index <- createDataPartition(data$CHURN, p = 0.1, list=FALSE)
index
datasample <- data[index,]
dim(datasample)
head(datasample)
summary(datasample$CHURN)

# Downsampling (Class variable is added as last column)
downsample <- downSample(data[, -1], data$CHURN, yname = "CHURN")
dim(downsample)
head(downsample)
summary(downsample$CHURN)

# 2.3	Pre-Processing Data
setwd("D:/Dropbox/Lehre/Digital Analytics/Software/R")
countries <- read.csv("europe.csv")
head(countries)
scaleddata <- scale(countries[,2:8])
head(scaleddata)

library(caret)
preProcValues <- preProcess(countries, method = c("center", "scale"))
preProcValues
# Apply them to the data sets:
scaleddata <- predict(preProcValues, countries)
head(scaleddata)

# revert the calculation:
reverseStandardize <- function(preProc, data){
  stopifnot(class(preProc) == "preProcess")
  stopifnot(class(data) == "data.frame")
  for(i in names(preProc$mean)){
    tmp <- data[, i] * preProc$std[[i]] + preProc$mean[[i]]
    data[, i] <- tmp
  }
  return(data)  
}

unscaled <- reverseStandardize(preProcValues, scaleddata)
head(unscaled)

preProcValues <- preProcess(countries, method = c("range"))
preProcValues
scaleddata <- predict(preProcValues, countries)
head(scaleddata)

reverseNormalize <- function(preProc, data){
  stopifnot(class(preProc) == "preProcess")
  stopifnot(class(data) == "data.frame")
  for(i in names(preProcValues$ranges[1,])){
    tmp <- data[, i] * (preProc$ranges[[2,i]] - preProc$ranges[[1,i]]) + preProc$ranges[[1,i]]
    data[, i] <- tmp
  }
  return(data)  
}

unnormalized <- reverseNormalize(preProcValues, scaleddata)
head(unnormalized)


# 2.4 Transforming Categorical in Dummy Variables
customers <- data.frame(id=c(10,20,30,40,50),
                        gender=as.factor(c('male','female','female','male','female')),
                        mood=as.factor(c('happy','sad','happy','sad','happy')),
                        outcome=c(1,1,0,0,0))
library(caret)
# transform all
dmy <- dummyVars(" ~ .", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)

# transform only gender
dmy <- dummyVars(" ~ gender", data = customers)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)

# transform all using the fullRank parameter
dmy <- dummyVars(" ~ .", data = customers, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = customers))
print(trsf)

# 2.5	Exploratory Data Analysis
summary(data)

# 2.6	Visualizing Data
setwd("D:/Dropbox/Lehre/Digital Analytics/Software/R")
countries <- read.csv("europe.csv")

plot(countries[,2:5])
 
plot(countries$Area, countries$GDP)
 
plot(countries$Area, countries$GDP, main="Plot", xlab="Area", ylab="GDP", type="l")
 
hist(countries$GDP)
 
boxplot(scale(countries[,2:5]))
 
# 2.7	Applying Principal Component Analysis
setwd("D:/Dropbox/Lehre/Digital Analytics/Software/R")
churn <- read.csv("churn-data.csv")
churn <- churn[,-1]
# apply PCA 
library(caret)
churn.caret = preProcess(churn, method=c("center", "scale", "pca"))
churn.caret
churn.new = predict(churn.caret, churn)
summary(churn.new)

# 2.8	Partitioning Data
setwd("D:/Dropbox/Lehre/Digital Analytics/Software/R")
data <- read.csv("churn-data.csv")
data$CHURN <- as.factor(data$CHURN)  # feature churn has to be converted into a categorical variable

# select 1000 records out from data
index <- sample(1:nrow(data), 1000, replace=FALSE)
index
datatrain <- data[index,]
datatest <- data[-index,]

# select 30% records out from data
index <- createDataPartition(data$CHURN, p = 0.3, list=FALSE)
index
datatrain <- data[index,]
datatest <- data[-index,]

index <- which(1:length(data[,1])%%2 == 0)
index
datatrain <- data[-index,]
datatest <- data[index,]

#regression 不需要


