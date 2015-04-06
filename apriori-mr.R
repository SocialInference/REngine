rm(list = ls()) # clear the history from the previous run (i.e. variables)
cat("\014")# Clear Console
closeAllConnections() # close any file connections if any
dev.off()# Clear All Graphs in the plot area

######Set the system parameters and environments variables
hcmd <-system("which hadoop", intern = TRUE)
Sys.setenv(HADOOP_CMD=hcmd)

hstreaming <- system("find /usr -name hadoop-streaming*jar", intern=TRUE)
Sys.setenv(HADOOP_STREAMING= hstreaming[1])

Sys.getenv("HADOOP_CMD")
Sys.getenv("HADOOP_STREAMING")

library(rmr2)
library (rhdfs)
#rmr.options(backend='hadoop')
rmr.options(backend='local')
hdfs.init()


library(arules)
data(AdultUCI)
################# Data Types
#### more information about the data can be found at https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names

#age: continuous.
#workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#fnlwgt: continuous.
#education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#education-num: continuous.
#marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#sex: Female, Male.
#capital-gain: continuous.
#capital-loss: continuous.
#hours-per-week: continuous.
#native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

################### Step 1 preparing the data
#### 1- remove less relevant variables

AdultUCI[["fnlwgt"]] <- NULL
AdultUCI[["education-num"]] <- NULL

####### 2- discretize and order contious variables into groups
AdultUCI[["age"]] <- ordered(cut(AdultUCI[["age"]], c(15, 25, 45, 65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

AdultUCI[["hours-per-week"]] <-ordered(cut(AdultUCI[["hours-per-week"]], c(0, 25, 40, 60, 168)), labels = c("Part-time","Full-time", "Over-time", "Workaholic"))

AdultUCI[["capital-gain"]]   <-ordered(cut(AdultUCI[["capital-gain"]],c(-Inf, 0, median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]] > 0]), Inf)),labels = c("None", "Low", "High"))

AdultUCI[["capital-loss"]]   <-ordered(cut(AdultUCI[["capital-loss"]],c(-Inf, 0, median(AdultUCI[["capital-loss"]][AdultUCI[["capital-loss"]] > 0]), Inf)), labels = c("none", "low", "high"))

### 3- Create dummy variables per factor (required by the Apriori algorithm 
Adult<-as(AdultUCI,'transactions')
Adult<-as(Adult,'matrix')
write.table(Adult[1:500,],row.names=FALSE,col.names=FALSE,sep=',', file='adult.csv')
to.dfs(Adult[1:500], output='adult.csv',format='csv')

my.form<-make.input.format(
  "csv", 
  sep = ",",
  col.names=colnames(Adult))

total<-500
min.support<-0.01

map.p1<-function(.,t) {
  d<-dim(t)
  cols<-colnames(t)
  size<-length(cols)
  vals<-matrix(nrow=size,ncol=d[1])
  for(i in 1:size) {
    vals[i,]<-t[,i]
  }
  keyval(cols,vals)
}

reduce.p1<-function(k,v) {
  s<-sum(v)
  support<-s/total
  if(support>=min.support) {
    keyval(k,support)
  }
}

mr.1<-mapreduce(input="adult.csv", map=map.p1, reduce=reduce.p1, input.format=my.form)
kvs<-cbind(keys(from.dfs(mr.1)),values(from.dfs(mr.1)))
kvs<-kvs[order(kvs[,2],kvs[,1]),]
View(kvs)

freqitems <- apriori(as(Adult[1:500,],'transactions'), parameter=list(support=0.01,maxlen=1,target='frequent itemsets'))
inspect(freqitems)
View(data.frame(kvs[,1],kvs[,2],as(freqitems,'data.frame')))



