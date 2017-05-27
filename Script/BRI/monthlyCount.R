monthlyCount <- function(x, y) {
  newDf <- data.frame(Month = factor(), Year = factor(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Mean = numeric())
  for (year in levels(x$Year)) {
    for (month in levels(x$Month)) {
      tmpdata <- sqldf(strwrap(sprintf("SELECT * FROM %s WHERE Year = %s AND Month = %s", y, year, month)))
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