

# To be able to call arules ruleInduction, we need to format data as an itemsets
# This method formats data in the form of a sparse array with a support column
# to an itemsets
# setMatrix is a matrix contains only the itemset definition. row = items, col = sets
# support is a vector defining the support for each itemset as a data.frame
toItemSets <- function(setMatrix, sup)
{
  itemz <- as(something, "itemMatrix")
  sets <- new("itemsets", items = itemz, quality = sup, tidLists=NULL)
  return sets
}

# test
#matrix <- matrix(c(0,1,1,1,
#                   0,1,1,0,
#                   0,0,1,1,
#                   0,1,0,1,
#                   0,1,0,0,
#                   0,0,1,0,
#                   0,0,0,1),
#                 nrow=4, 
#                 ncol=7
#                 )
#rownames(matrix) <- c("a","b","c","d")
#sup <- as.data.frame(c(.3,.4,.4,.4,.8,.8,.8))
#sets <- toItemSets(matrix,sup)
#inspect(sets)
