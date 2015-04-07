
##Do Something using the rules

##### WIP ######

#Example using Adult data
# predict Income
rules <- nrec
rules.sub <- subset(rules, subset = rhs %pin% "sex=")
inspect(rules.sub)

# assume predictors is a sparse row mapping
invokeRules <- function(scenario)
{
  subrules<-c()
  j = 1;
  for(i in 1:length(rules))
  {
    if(all(as(lhs(rules[i]), "matrix")==scenario))
    {
      subrules[j] <- rules[i]
      j = j+1
    }
  }
  return(subrules)
}
# get rules based on lhs matching transaction
newRules<-invokeRules(as(lhs(rules[1]), "matrix"))

# assume predictors is a list of items
invokeRulesList <- function(rules, predictors)
{
  # find matching rule using lhs (have predictor and nothing else)
  matchingRules <- lhs(rules) %in% predictors
  which(matchingRules == true)
  # return rhs of the index
  
}
# get rules based on lhs item
items <- c("relationship=Husband")
matchingRules <- lhs(rules) %in% items
which(matchingRules == TRUE)
inspect(rules.sub[1])

