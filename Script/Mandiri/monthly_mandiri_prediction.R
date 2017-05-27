#Sorting the data based on year done in excel
setwd("~/DSAFinalProject/")
mandiri <- read.csv("Data/bmri_2005.csv")

#Preprocessing data
mandiri <- mandiri[,-1]
mandiri$Mean <- rowMeans(mandiri[,5:6])
mandiri$Day <- as.numeric(mandiri$Day)
mandiri$Month <- factor(mandiri$Month)
mandiri$Year <- factor(mandiri$Year)
mandiri$Open <- as.numeric(mandiri$Open)
mandiri$High <- as.numeric(mandiri$High)
mandiri$Low <- as.numeric(mandiri$Low)
mandiri$Close <- as.numeric(mandiri$Close)
mandiri$Volume <- as.numeric(mandiri$Volume)
str(mandiri)

#Create mandiri monthly observation table
monthly_mandiri <- monthlyMandiri()

#Normalize the table
norm_monthly_mandiri <- as.data.frame(scale(monthly_mandiri[,3:8]))

#K-fold cross validation for checking errors
install.packages("cvTools", "e1071")
library(cvTools)
library(e1071)
k <- 5
folds <- cvFolds(NROW(norm_monthly_mandiri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_monthly_mandiri[folds$subsets[folds$which != i], ]
  data_test <- norm_monthly_mandiri[folds$subsets[folds$which == i], ]
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

#Add new row to norm_mandiri
last_open <- norm_monthly_mandiri$Open[nrow(norm_monthly_mandiri)]
last_high <- norm_monthly_mandiri$High[nrow(norm_monthly_mandiri)]
last_low <- norm_monthly_mandiri$Low[nrow(norm_monthly_mandiri)]
last_close <- norm_monthly_mandiri$Close[nrow(norm_monthly_mandiri)]
last_volume <- norm_monthly_mandiri$Volume[nrow(norm_monthly_mandiri)]
last_mean <- norm_monthly_mandiri$Mean[nrow(norm_monthly_mandiri)]
norm_monthly_mandiri[nrow(norm_monthly_mandiri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_monthly_mandiri[1:nrow(norm_monthly_mandiri)-1, ]
pred_test <- norm_monthly_mandiri[nrow(norm_monthly_mandiri), ]
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_monthly_mandiri$Mean[nrow(norm_monthly_mandiri)] <- res

#Visualize the data
norm_monthly_mandiri$ID <- seq.int(nrow(norm_monthly_mandiri))
attach(norm_monthly_mandiri)
plot(ID[nrow(norm_monthly_mandiri)-10:nrow(norm_monthly_mandiri)], Mean[nrow(norm_monthly_mandiri)-10:nrow(norm_monthly_mandiri)], main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
detach(norm_monthly_mandiri)
