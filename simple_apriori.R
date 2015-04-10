# Simple apriori on small data set
parameter <- list(support = 0.5, conf = 0.5,
                  minlen =1, maxlen=10,
                  target = "rules")

simpleData <- read.csv("/home/ict/R/REngine/gss24-trans-100r100c.csv", col.names=header)
simpleTrans <- as(as(simpleData, "matrix"), "transactions")
simpleRules <- apriori(simpleTrans, parameter)

# larger data set
newData <- read.csv("/home/ict/R/REngine/gss24-100c.csv", col.names=header)
newTrans <- as(as(newData, "matrix"), "transactions")
newRules <- apriori(newTrans, parameter = list(support = 0.5, conf = 0.9,
                                               minlen =6, maxlen=7,
                                               target = "rules"))
#plot(newRules, method="graph")
newSets <- apriori(newTrans, parameter <- list(support = 0.5, conf = 0.9,
                                               minlen =6, maxlen=7,
                                               target = "frequent itemsets"))
#plot(newSets)
# more support
xparam <- list(support = 0.7, conf = 0.9,
               minlen =1, maxlen=10,
               target = "rules")
xRules <- apriori(newTrans, xparam)
#summary(xRules)

# select rule set for analysis
rules <- newRules