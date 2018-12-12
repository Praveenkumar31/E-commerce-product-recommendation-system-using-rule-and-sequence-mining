##########################################
# simple demo of building sequence rules 
##########################################

library(arulesSequences)

data <- read_baskets(con = "C:/Users/91979/Desktop/web_analytics/a.txt", info = c("sequenceID","eventID","SIZE"))

as(head(data), "data.frame") # view first few rows of the data

seqs <- cspade(data, parameter = list(support = 0.1), control = list(verbose = TRUE))
as(seqs,"data.frame")  # view the sequences

rules <- ruleInduction(seqs, confidence = 0.5,control = list(verbose = TRUE))
as(rules,"data.frame")  # view the rules
