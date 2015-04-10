# Clean data

categorize <- function(aData)
{
  for(i in 1:length(aData))
  {
    if(is.numeric(aData[,i]))
    {
      # convert to numeric
      aData[,i] <- as.numeric(aData[,i])
      
      min <- min(aData[,i])
      max <- max(aData[,i])
      interval <- (max - min) / 5
      intervals <- c(min, min+interval, min+2*interval, min+3*interval, min+4*interval, max)
      
      labels <- c("Very Low", "Low", "Medium", "High", "Very High")
      
      # cut data
      aData[,i] <- tryCatch({ ordered(cut(aData[,i], intervals), labels = labels)
                            },
                            error = function (e) {
                              ordered(cut(aData[,i], intervals), labels = "L")
                            })
    }
  }
  return(aData)
}

format <- function(aData)
{
  return(as(aData, "transactions"))
}