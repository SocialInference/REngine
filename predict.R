library(combinat)
##Do Something using the rules

#Example using Adult data
# predict Income
rules <- nrec

# assume scenario is a sparse row mapping in matrix format
# uses feature set defined by scenario to find and output relevant rules
# finds exact matches to a rule
infer <- function(scenario)
{
  indeces <- which(lhs(rules) %in% as(scenario, "itemMatrix"))
  subrules <- rules[indeces]
  return(subrules)
}

# get rules based on lhs matching transaction
rules.infered<-infer(as(lhs(rules[12]), "matrix"))
inspect(rules.infered)
inspect(rhs(rules.infered))

# feature is in text format
# Finds what features affect this feature
whatAffects <- function(feature)
{
  subrules <- subset(rules, subset = rhs %pin% feature)
  return(subrules)
}

rules.affected <- whatAffects("capital-gain")
inspect(lhs(rules.affected))
