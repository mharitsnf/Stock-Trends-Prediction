bbni_2005$Mean <- rowMeans(bbni_2005[,5:6])
bbni_2005$Day <- format(as.Date(bbni_2005$Date,format="%d-%b-%y"),"%d")
bbni_2005$Day <- as.numeric(as.character(bbni_2005$Day))
bbni_2005$Month <- as.numeric(as.character(format(as.Date(bbni_2005$Date,format="%d-%b-%y"),"%m")))
bbni_2005$Year <- as.numeric(as.character(format(as.Date(bbni_2005$Date,format="%d-%b-%y"),"20%y")))

#Installing packages
install.packages("plyr")
install.packages("sqldf")
library(plyr)
library(sqldf)

#Preprocessing data
bbni_2005 <- bbni_2005[,-1]
bbni_2005 <- bbni_2005[order(nrow(bbni_2005):1),]
bbni_2005$ID <- seq.int(nrow(bbni_2005))
bbni_2005$Mean <- rowMeans(bbni_2005[,5:6])
bbni_2005$Day <- as.numeric(bbni_2005$Day)
bbni_2005$Month <- as.factor(bbni_2005$Month)
bbni_2005$Year <- as.factor(bbni_2005$Year)
bbni_2005$Open <- as.numeric(bbni_2005$Open)
bbni_2005$High <- as.numeric(bbni_2005$High)
bbni_2005$Low <- as.numeric(bbni_2005$Low)
bbni_2005$Close <- as.numeric(bbni_2005$Close)
bbni_2005$Volume <- as.numeric(bbni_2005$Volume)
#bbni_2005$MonthIndex <- revalue(bbni_2005$Month, c("5"="1", "6"="2", "7"="3", "8"="4", "9"="5", "10"="6", "11"="7", "12"="8", "1"="9", "2"="10", "3"="11", "4"="12"))
#bbni_2005$Month <- factor(bbni_2005$Month)
#bbni_2005$MonthIndex <- factor(bbni_2005$MonthIndex)
str(bbni_2005)

#Function for creating yearly observations (Aggregating)
yearlybbni_2005 <- function() {
  newDf <- data.frame(Year = factor(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Mean = numeric())
  for (year in levels(bbni_2005$Year)) {
    tmpdata <- sqldf(strwrap(sprintf("SELECT * FROM bbni_2005 WHERE Year = %s", year)))
    tmpOpen <- mean(tmpdata$Open)
    tmpHigh <- mean(tmpdata$High)
    tmpLow <- mean(tmpdata$Low)
    tmpClose <- mean(tmpdata$Close)
    tmpVolume <- mean(tmpdata$Volume)
    tmpMean <- mean(tmpdata$Mean)
    insertdf <- data.frame(Year = year, Open = tmpOpen, High = tmpHigh, Low = tmpLow, Close = tmpClose, Volume = tmpVolume, Mean = tmpMean)
    newDf <- rbind(newDf, insertdf)
  }
  return(newDf)
}
bbni_2005_year <- yearlybbni_2005()
bbni_2005_year$ID <- seq.int(nrow(bbni_2005_year))

#Data normalization
norm_bbni_2005_year <- as.data.frame(scale(bbni_2005_year[2:7]))

#K-Fold Cross Validation, checking errors
install.packages("cvTools")
install.packages( "e1071")
library(cvTools) #install package for k-fold cross validation
library(e1071)
k <- 10 #using 10 folds
folds <- cvFolds(NROW(norm_bbni_2005_year), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_bbni_2005_year[folds$subsets[folds$which != i], ]
  data_test <- norm_bbni_2005_year[folds$subsets[folds$which == i], ]
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

#Add new row to norm_bbni_2005_year
last_open <- norm_bbni_2005_year$Open[nrow(norm_bbni_2005_year)]
last_high <- norm_bbni_2005_year$High[nrow(norm_bbni_2005_year)]
last_low <- norm_bbni_2005_year$Low[nrow(norm_bbni_2005_year)]
last_close <- norm_bbni_2005_year$Close[nrow(norm_bbni_2005_year)]
last_volume <- norm_bbni_2005_year$Volume[nrow(norm_bbni_2005_year)]
last_mean <- norm_bbni_2005_year$Mean[nrow(norm_bbni_2005_year)]
norm_bbni_2005_year[nrow(norm_bbni_2005_year) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_bbni_2005_year[1:nrow(norm_bbni_2005_year)-1, ] #training data using all but the newest data
pred_test <- norm_bbni_2005_year[nrow(norm_bbni_2005_year), ] #testing data using the last data
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
res
norm_bbni_2005_year$Mean[nrow(norm_bbni_2005_year)] <- res

#Plotting
norm_bbni_2005_year$ID <- seq.int(nrow(norm_bbni_2005_year))
attach(norm_bbni_2005_year)
plot(ID, Mean, main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
