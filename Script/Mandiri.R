#Before doing this, I've do some wrangling: sorting the data
#based on year
setwd("~/DSAFinalProject/")
mandiri <- read.csv("Data/bmri_2005.csv")

#Installing packages
install.packages("plyr", "sqldf")
library(plyr)
library(sqldf)

#Preprocessing data
mandiri <- mandiri[,-1]
mandiri$ID <- seq.int(nrow(mandiri))
mandiri$Mean <- rowMeans(mandiri[,5:6])
mandiri$Day <- as.numeric(mandiri$Day)
mandiri$Month <- as.character(mandiri$Month)
mandiri$Year <- factor(mandiri$Year)
mandiri$Open <- as.numeric(mandiri$Open)
mandiri$High <- as.numeric(mandiri$High)
mandiri$Low <- as.numeric(mandiri$Low)
mandiri$Close <- as.numeric(mandiri$Close)
mandiri$Volume <- as.numeric(mandiri$Volume)
#mandiri$MonthIndex <- revalue(mandiri$Month, c("5"="1", "6"="2", "7"="3", "8"="4", "9"="5", "10"="6", "11"="7", "12"="8", "1"="9", "2"="10", "3"="11", "4"="12"))
#mandiri$Month <- factor(mandiri$Month)
#mandiri$MonthIndex <- factor(mandiri$MonthIndex)
str(mandiri)

#Function for creating yearly observations (Aggregating)
yearlyMandiri <- function() {
  newDf <- data.frame(Year = factor(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Mean = numeric())
  for (year in levels(mandiri$Year)) {
    tmpdata <- sqldf(strwrap(sprintf("SELECT * FROM mandiri WHERE Year = %s", year)))
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
mandiri_year <- yearlyMandiri()
mandiri_year$ID <- seq.int(nrow(mandiri_year))

#Function for creating monthly observation (Aggregating)
monthlyMandiri <- function() {
  newDf <- data.frame(Month = factor(), Year = factor(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Mean = numeric())
  for (year in levels(mandiri$Year)) {
    for (month in levels(mandiri$Month)) {
      tmpdata <- sqldf(strwrap(sprintf("SELECT * FROM mandiri WHERE Year = %s AND Month = %s", year, month)))
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
mandiri_monthly <- monthlyMandiri()
mandiri_monthly$ID <- seq.int(nrow(mandiri_monthly))


#Data normalization
norm_mandiri_year <- as.data.frame(scale(mandiri_year[2:7]))

#Making models and doing regression
set.seed(666)
n <- length(norm_mandiri_year$Mean)
n1 <- 10
n2 <- n-n1
train <- sample(1:n,n1)
model1 <- lm(Mean~Volume, norm_mandiri_year[train, ])
summary(model1)
pred <- predict(model1, newdat = norm_mandiri_year[-train,])
obs <- norm_mandiri_year$Mean[-train]
diff <- obs-pred
percdiff <- abs(diff)/obs
me <- mean(diff)
rmse <- sqrt(sum(diff**2)/n2)
mape <- 100*(mean(percdiff))
me
rmse
mape

#Plotting
plot(ID, Mean, main="Scatterplot Example", xlab="Time ", ylab="Mean ", pch=20)
abline(lm(Mean~ID), col="red")
