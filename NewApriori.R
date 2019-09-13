library(data.table)
library("plyr")
library(dplyr)
library(arules)
library("xlsx")

## paras
len.m <- c(1:3)
supp.all <- 250
supp.myo <- 1

datadir <- "/Users/julielee/DDI/"
setwd(datadir)



fn <- 'drugid_only.txt'
data.raw <- fread(fn, header = T, sep = '\t')$GroupID
idx.nna <- which(!is.na(data.raw))
data <- data.raw[idx.nna]
data.list<- strsplit(data, "[ ]+")
names(data.list) <- paste("Tr", c(1:length(data.list)), sep = "")

## read event data
event.raw <- fread('FAERS_ADE.txt', header = F, sep = '\n' )
event <- event.raw[idx.nna, ]
idx.myo <- which(event$V1 == 'Myopathy')
data.list.myo <- data.list[idx.myo]
data.list.myo

## ------------------------------------------------------------------------------------
## apriori on all 
tr.all <- as(data.list, 'transactions')
save(tr.all, file = "all_transactions.csv")

association.rules.all <- apriori(tr.all, parameter = list(supp = (supp.all/length(data.list)), minlen = len.m[1], maxlen = len.m[length(len.m)], maxtime = 0, target = "frequent itemsets"))
association.rules.all.df <- (as(association.rules.all, 'data.frame')) %>% 
  arrange(count, items)
fwrite(association.rules.all.df, file = "assocation_rules.csv")

## apriori on myo cases
tr.myo <- as(data.list.myo, 'transactions')
save(tr.myo, file = "m_association_rules.csv")

association.rules.myo <- apriori(tr.myo, parameter = list(supp = (supp.myo/length(data.list.myo)), minlen = len.m[1], maxlen = len.m[length(len.m)], maxtime = 0, target = "frequent itemsets"))
association.rules.myo.df <- (as(association.rules.myo, 'data.frame')) %>% 
  arrange(count, items)
fwrite(association.rules.myo.df, file = "everything.csv")