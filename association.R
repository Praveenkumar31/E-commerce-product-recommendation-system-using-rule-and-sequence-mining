#######################################################################
# simple demo of testing a set of association rules against a separate test set
#######################################################################

library("arules");
getwd()
setwd("C:\\Users\\91979\\Desktop\\Web_analytics_V2.0")
#build rules
assoc_buy = read.transactions(file="./data/assoc_r_buy_data.csv",rm.duplicates=TRUE, format="single", sep=",", cols=c("session_id","item_id"));
assoc_buy
rules <- apriori(assoc_buy, parameter = list(supp=0.01, conf=0.2, minlen=2))
summary(rules)
inspect(rules)
#inspect(sort(rules, by = 'lift')[1:10])

# a useful plot of training data
itemFrequencyPlot(assoc_buy,topN=20,type="absolute")

#read the test data
assoc_buy = read.csv(file="./data/assoc_r_test.csv");
colnames(assoc_buy) <- c("session_id","item_id")  # set standard names, in case they are different in the data file

#execute rules against test data
makepreds <- function(itemid, rulesDF) {
  antecedent = paste("{",itemid,"} =>",sep="") 
  firingrules = rulesDF[grep(antecedent, rulesDF$rules,fixed=TRUE),1]
  gsub(" ","",toString(sub("\\}","",sub(".*=> \\{","",firingrules))))
}

rulesDF = as(rules,"data.frame")
assoc_buy$preds = apply(assoc_buy,1,function(X) makepreds(X["item_id"], rulesDF))


# extract unique predictions for each test user
#remove duplicate item_id from a basket (itemstrg)
uniqueitems <- function(itemstrg) {
  unique(as.list(strsplit(gsub(" ","",itemstrg),","))[[1]])
}
userpreds = as.data.frame(aggregate(preds ~ session_id, data = assoc_buy, paste, collapse=","))
userpreds$preds = apply(userpreds,1,function(X) uniqueitems(X["preds"]))

# extract unique item_id bought (or rated highly) for each test user
baskets = as.data.frame(aggregate(item_id ~ session_id, data = assoc_buy, paste, collapse=","))
baskets$item_id = apply(baskets,1,function(X) uniqueitems(X["item_id"]))

checkpreds <- function(preds, baskID) {
  plist = preds[[1]]
  blist = baskets[baskets$session_id == baskID,"item_id"][[1]]
  cnt = 0 
  for (p in plist) {
    if (p %in% blist) cnt = cnt+1
  }
  cnt
}

#count how many unique predictions made are correct, i.e. have previously been bought (or rated highly) by the user
correctpreds = sum(apply(userpreds,1,function(X) checkpreds(X["preds"],X["session_id"])))

# count all predictions made
countpreds <- function(predlist) {
  len = length(predlist)
  if (len > 0 && (predlist[[1]] == "")) 0 # avoid counting an empty list
  else len
}

totalpreds = sum(apply(userpreds,1,function(X) countpreds(X["preds"][[1]]))) 

precision = correctpreds*100/totalpreds

cat("precision=", precision, "corr=",correctpreds,"total=",totalpreds)


library(arulesViz)
plot(rules)
plot(rules, method="graph")
plot(rules, method="graph",nodeCol=grey.colors(10),edgeCol=grey(.7),alpha=1)
plot(rules, method="matrix")
plot(rules, method="paracoord", control=list(reorder=TRUE))


