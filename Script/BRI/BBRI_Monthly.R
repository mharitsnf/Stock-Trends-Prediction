bbri_2005$Mean <- rowMeans(bbri_2005[,5:6])
bbri_2005$Day <- format(as.Date(bbri_2005$Date,format="%d-%b-%y"),"%d")
bbri_2005$Day <- as.numeric(as.character(bbri_2005$Day))
bbri_2005$Month <- as.numeric(as.character(format(as.Date(bbri_2005$Date,format="%d-%b-%y"),"%m")))
bbri_2005$Year <- as.numeric(as.character(format(as.Date(bbri_2005$Date,format="%d-%b-%y"),"20%y")))

#Installing packages
install.packages("plyr")
install.packages("sqldf")
library(plyr)
library(sqldf)

#Preprocessing data
bbri_2005 <- bbri_2005[,-1]
bbri_2005 <- bbri_2005[order(nrow(bbri_2005):1),]
bbri_2005$ID <- seq.int(nrow(bbri_2005))
bbri_2005$Mean <- rowMeans(bbri_2005[,5:6])
bbri_2005$Day <- as.numeric(bbri_2005$Day)
bbri_2005$Month <- as.factor(bbri_2005$Month)
bbri_2005$Year <- as.factor(bbri_2005$Year)
bbri_2005$Open <- as.numeric(bbri_2005$Open)
bbri_2005$High <- as.numeric(bbri_2005$High)
bbri_2005$Low <- as.numeric(bbri_2005$Low)
bbri_2005$Close <- as.numeric(bbri_2005$Close)
bbri_2005$Volume <- as.numeric(bbri_2005$Volume)
#bbri_2005$MonthIndex <- revalue(bbri_2005$Month, c("5"="1", "6"="2", "7"="3", "8"="4", "9"="5", "10"="6", "11"="7", "12"="8", "1"="9", "2"="10", "3"="11", "4"="12"))
#bbri_2005$Month <- factor(bbri_2005$Month)
#bbri_2005$MonthIndex <- factor(bbri_2005$MonthIndex)
str(bbri_2005)

#Function for creating monthly observation (Aggregating)
monthlybbri_2005 <- function() {
  newDf <- data.frame(Month = factor(), Year = factor(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Mean = numeric())
  for (year in levels(bbri_2005$Year)) {
    for (month in levels(bbri_2005$Month)) {
      tmpdata <- sqldf(strwrap(sprintf("SELECT * FROM bbri_2005 WHERE Year = %s AND Month = %s", year, month)))
      tmpOpen <- mean(tmpdata$Open)
      tmpHigh <- mean(tmpdata$High)
      tmpLow <- mean(tmpdata$Low)
      tmpClose <- mean(tmpdata$Close)
      tmpVolume <- mean(tmpdata$Volume)
      tmpMean <- mean(tmpdata$Mean)
      insertdf <- data.frame(Month = month, Year = year, Open = tmpOpen, High = tmpHigh, Low = tmpLow, Close = tmpClose, Volume = tmpVolume, Mean = tmpMean)
      newDf <- rbind(newDf, insertdf)
    }
  }
  newDf <- na.omit(newDf)
  return(newDf)
}
bbri_2005_monthly <- monthlybbri_2005()
bbri_2005_monthly$ID <- seq.int(nrow(bbri_2005_monthly))


#Data normalization
norm_bbri_2005_monthly <- as.data.frame(scale(bbri_2005_monthly[3:8]))

#K-Fold Cross Validation, checking errors
install.packages("cvTools")
install.packages( "e1071")
library(cvTools) #install package for k-fold cross validation
library(e1071)
k <- 10 #using 10 folds
folds <- cvFolds(NROW(norm_bbri_2005_monthly), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_bbri_2005_monthly[folds$subsets[folds$which != i], ]
  data_test <- norm_bbri_2005_monthly[folds$subsets[folds$which == i], ]
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
me
rmse
mape
errors

#Add new row to norm_bbri_2005_monthly
last_open <- norm_bbri_2005_monthly$Open[nrow(norm_bbri_2005_monthly)]
last_high <- norm_bbri_2005_monthly$High[nrow(norm_bbri_2005_monthly)]
last_low <- norm_bbri_2005_monthly$Low[nrow(norm_bbri_2005_monthly)]
last_close <- norm_bbri_2005_monthly$Close[nrow(norm_bbri_2005_monthly)]
last_volume <- norm_bbri_2005_monthly$Volume[nrow(norm_bbri_2005_monthly)]
last_mean <- norm_bbri_2005_monthly$Mean[nrow(norm_bbri_2005_monthly)]
norm_bbri_2005_monthly[nrow(norm_bbri_2005_monthly) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_bbri_2005_monthly[1:nrow(norm_bbri_2005_monthly)-1, ] #training data using all but the newest data
pred_test <- norm_bbri_2005_monthly[nrow(norm_bbri_2005_monthly), ] #testing data using the last data
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
res
norm_bbri_2005_monthly$Mean[nrow(norm_bbri_2005_monthly)] <- res

#Plotting
norm_bbri_2005_monthly$ID <- seq.int(nrow(norm_bbri_2005_monthly))
attach(norm_bbri_2005_monthly)
plot(norm_bbri_2005_monthly$ID, norm_bbri_2005_monthly$Mean, main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(norm_bbri_2005_monthly$Mean~norm_bbri_2005_monthly$ID), col="red")
