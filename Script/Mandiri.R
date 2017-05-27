#Before doing this, I've do some wrangling: sorting the data
#based on year
setwd("~/DSAFinalProject/")
mandiri <- read.csv("Data/bmri_2005.csv")

#Preprocessing data
mandiri$Volume <- as.numeric(mandiri$Volume)
mandiri <- mandiri[,-1]
mandiri$Mean <- rowMeans(mandiri[,5:6])
mandiri$Day <- factor(mandiri$Day)
mandiri$Month <- factor(mandiri$Month)
mandiri$Year <- factor(mandiri$Year)
mandiri$Open <- as.numeric(mandiri$Open)
mandiri$High <- as.numeric(mandiri$High)
mandiri$Low <- as.numeric(mandiri$Low)
mandiri$Close <- as.numeric(mandiri$Close)
str(mandiri)

#Install sql package for ease querying
install.packages('sqldf')
library(sqldf)

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

#Data normalization
norm_mandiri_year <- as.data.frame(scale(mandiri_year[2:7]))

#Making models and doing regression
model1 <- lm(Mean~Low+Volume, norm_mandiri_year)
summary(model1)
set.seed(666)
n <- length(norm_mandiri_year$Mean)
n1 <- 10
n2 <- n-n1
train <- sample(1:n,n1)
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
