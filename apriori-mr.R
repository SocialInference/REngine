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
write.table(Adult[1:1000,],row.names=FALSE,col.names=FALSE,sep=',', file='adult.csv')
dfs.rmr('adult.csv')

my.out.form<-make.output.format(
  "csv", 
  sep = ",")
to.dfs(Adult[1:1000,], output='adult.csv',format=my.out.form)

my.form<-make.input.format(
  "csv", 
  sep = ",",
  col.names=colnames(Adult))

total<-1000
min.support<-0.01

map.p1<-function(.,t) {
  d<-dim(t)
  size<-d[2]
  vals<-matrix(nrow=size,ncol=d[1])
  keys<-numeric(0)
  for(i in 1:size) {
    vals[i,]<-t[,i]
    itemset<-numeric(size)
    itemset[i]<-1
    keys<-rbind(keys,itemset)
  }
  keydf<-as.data.frame(keys)
  colnames(keydf)<-colnames(t)
  keyval(keydf,vals)
}

reduce.p1<-function(k,v) {
  s<-sum(v)
  support<-s/1000
  if(support>=0.01) {
    keyval(k,support)
  }
}

mr.1<-mapreduce(input="adult.csv", map=map.p1,reduce=reduce.p1, input.format=my.form)
itemsets.1<-keys(from.dfs(mr.1))
support.1<-values(from.dfs(mr.1))
View(itemsets.1)
View(support.1)

freqitems <- apriori(as(Adult[1:1000,],'transactions'), parameter=list(support=0.01,maxlen=1,target='frequent itemsets'))
inspect(freqitems)
View(data.frame(kvs[,1],kvs[,2],as(freqitems,'data.frame')))

ap_gen<-function(items) {
  dimitems<-dim(items)
  numsets<-dimitems[1]
  numcols<-dimitems[2]
  candidates<-numeric(0)
  
  # 1-itemsets are an exception
  if(sum(items[1,]) == 1) {
    for(i in 1:numsets) {
      for(j in i:numsets) {
        if(i != j) {
          prospect<-items[i,] + items[j,]
          candidates<-rbind(candidates,prospect)
        }
      }
    }
  }
  else {
    for(col in 1:numcols) {
      #browser()
      # finding a row to compare and generate union sets off of
      testrow<-0
      for(r in 1:numsets) {
        
        # looking for leading one row for the current column
        # This eliminates duplicate candidates from being generated
        leadingone<-numeric(col)
        leadingone[col]<-1
        for(i in 1:col) {
          if(items[r,i] != leadingone[i]) {
            testrow<-0
            break
          } else {
            testrow<-r
          }
        }
        if(testrow!=0) break
      }
      
      if(testrow!=0) {
        # Comparing rows and adding as a way to generate candidates
        for(r in 1:numsets) {
          
          leadingone<-numeric(col)
          leadingone[col]<-1
          # Look for a leading one row for current column to properly add similar sets
          for(i in 1:col) {
            if(items[r,i] != leadingone[i]) {
              comparerow<-0
              break
            } else {
              comparerow<-r
            }
          }
          
          if(comparerow!=0 && comparerow!=testrow) {
            prospect<-items[r,] + items[testrow,]
            prospect<-ifelse(prospect==2,1,prospect)
            if(!has_infrequent_subset(prospect,items)) {
              candidates<-rbind(candidates,prospect)
            }
          }
        }
      }
    }
  }
 
  candidates
}

has_infrequent_subset<-function(set, items) {
  has_infrequent<-TRUE
  # To generate subsets to check just turn off a one and check if that item is in the itemset
  for(i in 1:length(set)) {
    if(set[i] == 1) {
      testset<-set
      testset[i]<-0
      
      for(j in dim(items)[1]) {
        if(all(testset==items[j,])) {
          has_infrequent<-FALSE
          break
        }
      }
    }
  }
  has_infrequent
}


