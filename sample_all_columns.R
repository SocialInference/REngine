
# read data into frame
moreData <- as.data.frame(data, colClassses = "factor")

# remove ID column
mData <- moreData[-1]

# convert numericals to factors
cleanedData <- categorize(mData)

# take a small sample for testing
cutData <- head(cleanedData, 1)

# format it for apriori
cutTrans <- format(cutData)

# run apriori algorithm
start < proc.time()
cutRules <- apriori(cutTrans, parameter = list(support = 0.9, conf = 0.9,
                                               minlen =6, maxlen=7,
                                               target = "rules"))
proc.time() - start
# Too many columns - R session crashes