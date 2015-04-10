# Clean data

for(i in 1:length(mData))
{
  if(is.numeric(mData[,i]))
  {
    # convert to numeric
    mData[,i] <- as.numeric(mData[,i])
    
    min <- min(mData[,i])
    max <- max(mData[,i])
    interval <- (max - min) / 5
    intervals <- c(min, min+interval, min+2*interval, min+3*interval, min+4*interval, max)
    
    # create labels for existing categories
    labels <- c("Very Low")
    if(any(mData[,i] >= intervals[2] && mData[,i] < intervals[3])) c(labels, "Low")
    if(any(mData[,i] >= intervals[3] && mData[,i] < intervals[4])) c(labels, "Medium")
    if(any(mData[,i] >= intervals[4] && mData[,i] < intervals[5])) c(labels, "High")
    c(labels,"Very High")
    
    # cut data
    mData[,i] <- ordered(cut(mData[,i], intervals), labels = labels)
  }
}