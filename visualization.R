library(arulesViz)
library(igraph)

igraph.options(size=1, size2=1, arrow.size=0.2, curved = TRUE, labels = TRUE)
plot(rules,method="graph")
plot(rules)