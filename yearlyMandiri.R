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
