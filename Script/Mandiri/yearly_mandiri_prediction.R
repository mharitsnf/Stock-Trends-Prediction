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
yearly_mandiri <- yearlyMandiri()

#Normalize the table
norm_yearly_mandiri <- as.data.frame(scale(yearly_mandiri[,2:7]))

#K-fold cross validation for checking errors
install.packages("cvTools", "e1071")
library(cvTools)
library(e1071)
k <- 3
folds <- cvFolds(NROW(norm_yearly_mandiri), K=k)
errors <- data.frame(me = double(), rmse = double(), mape = double()) #dataframe for collecting the errors
for(i in 1:k){
  data_train <- norm_yearly_mandiri[folds$subsets[folds$which != i], ]
  data_test <- norm_yearly_mandiri[folds$subsets[folds$which == i], ]
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
last_open <- norm_yearly_mandiri$Open[nrow(norm_yearly_mandiri)]
last_high <- norm_yearly_mandiri$High[nrow(norm_yearly_mandiri)]
last_low <- norm_yearly_mandiri$Low[nrow(norm_yearly_mandiri)]
last_close <- norm_yearly_mandiri$Close[nrow(norm_yearly_mandiri)]
last_volume <- norm_yearly_mandiri$Volume[nrow(norm_yearly_mandiri)]
last_mean <- norm_yearly_mandiri$Mean[nrow(norm_yearly_mandiri)]
norm_yearly_mandiri[nrow(norm_yearly_mandiri) + 1, ] <- c(last_open, last_high, last_low, last_close, last_volume, last_mean)

#Predicting
pred_train <- norm_yearly_mandiri[1:nrow(norm_yearly_mandiri)-1, ]
pred_test <- norm_yearly_mandiri[nrow(norm_yearly_mandiri), ]
pred_model <- lm(Mean ~ Volume + Low, pred_train)
res <- predict(pred_model, newdat=pred_test)
norm_yearly_mandiri$Mean[nrow(norm_yearly_mandiri)] <- res

#Visualize the data
norm_yearly_mandiri$ID <- seq.int(nrow(norm_yearly_mandiri))
attach(norm_yearly_mandiri)
plot(ID, Mean, main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
detach(norm_yearly_mandiri)