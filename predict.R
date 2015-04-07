
##Do Something using the rules

#Example using Adult data
# predict Income
rules <- nrec

# assume scenario is a sparse row mapping in matrix format
# uses feature set defined by scenario to find and output relevant rules
infer <- function(scenario)
{
  indeces <- which(lhs(rules) %in% as(scenario, "itemMatrix"))
  subrules <- rules[indeces]
  return(subrules)
}

# get rules based on lhs matching transaction
newRules<-infer(as(lhs(rules[12]), "matrix"))
inspect(newRules)
inspect(rhs(newRules))

