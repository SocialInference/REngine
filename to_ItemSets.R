# To be able to call arules ruleInduction, we need to format data as an itemsets
# This method formats data in the form of a sparse array with a support column
# to an itemsets
# setMatrix is a matrix contains only the itemset definition. row = items, col = sets
# support is a vector defining the support for each itemset as a data.frame
toItemSets <- function(setMatrix, sup)
{
  itemz <- as(setMatrix, "itemMatrix")
  sets <- new("itemsets", items = itemz, quality = sup, tidLists=NULL)
  return(sets)
}

# test
matrix <- matrix(c(0,0,0,1,
                   0,0,1,0,
                   0,0,1,1),
                 nrow=3, 
                 ncol=4
                 )
colnames(matrix) <- c("a","b","c","d")
sup <- as.data.frame(c(.1, .3, .3))
sets <- toItemSets(matrix,sup)
inspect(sets)
ruleInduction(sets)


ec  <- eclat(Adult, parameter = list(support = 0.4))
rec <- ruleInduction(ec)
inspect(rec[1:5])