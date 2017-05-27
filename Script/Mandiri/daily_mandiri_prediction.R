#Sorting the data based on year done in excel
setwd("~/DSAFinalProject/")
mandiri <- read.csv("Data/bmri_2005.csv")

#Installing packages
install.packages("plyr", "sqldf")
library(plyr) #for data manipulation
library(sqldf) #for querying with sql syntax

#Preprocessing data
mandiri <- mandiri[,-1] #deleting
mandiri$Mean <- rowMeans(mandiri[,5:6]) #Calculating mean from high and low feature
mandiri$ID <- seq.int(nrow(mandiri)) #adding ID column and changing data types
mandiri$Day <- as.numeric(mandiri$Day)
mandiri$Month <- as.numeric(mandiri$Month)
mandiri$Year <- as.numeric(mandiri$Year)
mandiri$Open <- as.numeric(mandiri$Open)
mandiri$High <- as.numeric(mandiri$High)
mandiri$Low <- as.numeric(mandiri$Low)
mandiri$Close <- as.numeric(mandiri$Close)
mandiri$Volume <- as.numeric(mandiri$Volume)
str(mandiri)

#Data normalization
norm_mandiri <- as.data.frame(scale(mandiri[4:9])) #normalize the data

#K-Fold Cross Validation, checking errors
install.packages("cvTools", "e1071")
library(cvTools) #install package for k-fold cross validation
library(e1071)
k <- 10 #using 10 folds
folds <- cvFolds(NROW(norm_mandiri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_mandiri[folds$subsets[folds$which != i], ]
  data_test <- norm_mandiri[folds$subsets[folds$which == i], ]
  model <- lm(Mean ~ Volume + Low, data_train) #making model from training
  pred <- predict(model, newdat=data_test) #making model from testing
  obs <- data_test$Mean
  diff <- obs-pred
  percdiff <- abs(diff)/obs
  me <- mean(diff) #calculating errors
  rmse <- sqrt(sum(diff**2)/nrow(data_test))
  mape <- 100*(mean(percdiff))
  error_row <- data.frame(me = me, rmse = rmse, mape = mape)
  errors <- rbind(errors, error_row)
}

#Add new row to norm_mandiri
last_open <- norm_mandiri$Open[nrow(norm_mandiri)]
last_high <- norm_mandiri$High[nrow(norm_mandiri)]
last_low <- norm_mandiri$Low[nrow(norm_mandiri)]
last_close <- norm_mandiri$Close[nrow(norm_mandiri)]
last_volume <- norm_mandiri$Volume[nrow(norm_mandiri)]
last_mean <- norm_mandiri$Mean[nrow(norm_mandiri)]
norm_mandiri[nrow(norm_mandiri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_mandiri[1:nrow(norm_mandiri)-1, ] #training data using all but the newest data
pred_test <- norm_mandiri[nrow(norm_mandiri), ] #testing data using the last data
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_mandiri$Mean[nrow(norm_mandiri)] <- res #result prediction inserted into normalized data

#Plotting
norm_mandiri$ID <- seq.int(nrow(norm_mandiri))
attach(norm_mandiri)
plot(ID, Mean, main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")