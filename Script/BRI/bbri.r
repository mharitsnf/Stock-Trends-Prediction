#Sorting the data based on year done in excel
setwd("~/DSAFinalProject/")
bbri <- read.csv("Data/bbri_2005.csv")

#Installing packages
install.packages("plyr")
("sqldf")
library(plyr) #for data manipulation
library(sqldf) #for querying with sql syntax

#Preprocessing data
bbri <- bbri[order(nrow(bbri):1),]
bbri <- bbri[,-1] #deleting
bbri$Mean <- rowMeans(bbri[,2:3]) #Calculating mean from high and low feature
bbri$ID <- seq.int(nrow(bbri)) #adding ID column and changing data types
bbri$Day <- as.numeric(bbri$Day)
bbri$Month <- as.factor(bbri$Month)
bbri$Year <- as.factor(bbri$Year)
bbri$Open <- as.numeric(bbri$Open)
bbri$High <- as.numeric(bbri$High)
bbri$Low <- as.numeric(bbri$Low)
bbri$Close <- as.numeric(bbri$Close)
bbri$Volume <- as.numeric(bbri$Volume)
str(bbri)
#Create mandiri monthly observation table
yearly_bbri <- yearlyCount(bbri, "bbri")
#Create mandiri monthly observation table
monthly_bbri <- monthlyCount(bbri, "bbri")

#Data normalization
norm_bbri <- as.data.frame(scale(bbri[,1:6])) #normalize the data
#Normalize the table
norm_monthly_bbri <- as.data.frame(scale(monthly_bbri[,3:8]))
#Normalize the table
norm_yearly_bbri <- as.data.frame(scale(yearly_bbri[,2:7]))

#K-Fold Cross Validation, checking errors
install.packages("cvTools")
install.packages("e1071")
library(cvTools) #install package for k-fold cross validation
library(e1071)
k <- 10 #using 10 folds
folds <- cvFolds(NROW(norm_bbri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_bbri[folds$subsets[folds$which != i], ]
  data_test <- norm_bbri[folds$subsets[folds$which == i], ]
  model <- lm(Mean ~ Volume + Low, data_train) #making model from training
  pred <- predict(model, newdat=data_test) #making model from testing
  obs <- data_test$Mean
  diff <- obs-pred
  percdiff <- abs(diff)/abs(obs)
  me <- mean(diff) #calculating errors
  rmse <- sqrt(sum(diff**2)/nrow(data_test))
  mape <- 100*(mean(percdiff))
  error_row <- data.frame(me = me, rmse = rmse, mape = mape)
  errors <- rbind(errors, error_row)
}
errors
mean(errors$mape)

#Add new row to norm_bbri
last_open <- norm_bbri$Open[nrow(norm_bbri)]
last_high <- norm_bbri$High[nrow(norm_bbri)]
last_low <- norm_bbri$Low[nrow(norm_bbri)]
last_close <- norm_bbri$Close[nrow(norm_bbri)]
last_volume <- norm_bbri$Volume[nrow(norm_bbri)]
last_mean <- norm_bbri$Mean[nrow(norm_bbri)]
norm_bbri[nrow(norm_bbri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_bbri[1:nrow(norm_bbri)-1, ] #training data using all but the newest data
pred_test <- norm_bbri[nrow(norm_bbri), ] #testing data using the last data
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_bbri$Mean[nrow(norm_bbri)] <- res

#Plotting
norm_bbri$ID <- seq.int(nrow(norm_bbri))
attach(norm_bbri)
plot(ID, Mean, main="Trend Saham Bank BRI", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")

k <- 5
folds <- cvFolds(NROW(norm_monthly_bbri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_monthly_bbri[folds$subsets[folds$which != i], ]
  data_test <- norm_monthly_bbri[folds$subsets[folds$which == i], ]
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
errors
mean(errors$mape)

#Add new row to norm_monthly_bbri
last_open <- norm_monthly_bbri$Open[nrow(norm_monthly_bbri)]
last_high <- norm_monthly_bbri$High[nrow(norm_monthly_bbri)]
last_low <- norm_monthly_bbri$Low[nrow(norm_monthly_bbri)]
last_close <- norm_monthly_bbri$Close[nrow(norm_monthly_bbri)]
last_volume <- norm_monthly_bbri$Volume[nrow(norm_monthly_bbri)]
last_mean <- norm_monthly_bbri$Mean[nrow(norm_monthly_bbri)]
norm_monthly_bbri[nrow(norm_monthly_bbri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_monthly_bbri[1:nrow(norm_monthly_bbri)-1, ]
pred_test <- norm_monthly_bbri[nrow(norm_monthly_bbri), ]
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_monthly_bbri$Mean[nrow(norm_monthly_bbri)] <- res

#Visualize the data
norm_monthly_bbri$ID <- seq.int(nrow(norm_monthly_bbri))
attach(norm_monthly_bbri)
plot(ID[nrow(norm_monthly_bbri)-10:nrow(norm_monthly_bbri)], Mean[nrow(norm_monthly_bbri)-10:nrow(norm_monthly_bbri)], main="Trend Saham Bank BRI per Bulan", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
detach(norm_monthly_bbri)

k <- 3
folds <- cvFolds(NROW(norm_yearly_bbri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_yearly_bbri[folds$subsets[folds$which != i], ]
  data_test <- norm_yearly_bbri[folds$subsets[folds$which == i], ]
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
errors
mean(errors$mape)

#Add new row to norm_yearly_bbri
last_open <- norm_yearly_bbri$Open[nrow(norm_yearly_bbri)]
last_high <- norm_yearly_bbri$High[nrow(norm_yearly_bbri)]
last_low <- norm_yearly_bbri$Low[nrow(norm_yearly_bbri)]
last_close <- norm_yearly_bbri$Close[nrow(norm_yearly_bbri)]
last_volume <- norm_yearly_bbri$Volume[nrow(norm_yearly_bbri)]
last_mean <- norm_yearly_bbri$Mean[nrow(norm_yearly_bbri)]
norm_yearly_bbri[nrow(norm_yearly_bbri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_yearly_bbri[1:nrow(norm_yearly_bbri)-1, ]
pred_test <- norm_yearly_bbri[nrow(norm_yearly_bbri), ]
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_yearly_bbri$Mean[nrow(norm_yearly_bbri)] <- res

#Visualize the data
norm_yearly_bbri$ID <- seq.int(nrow(norm_yearly_bbri))
attach(norm_yearly_bbri)
plot(ID, Mean, main="Trend Saham Bank BRI per Tahun", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
detach(norm_yearly_bbri)