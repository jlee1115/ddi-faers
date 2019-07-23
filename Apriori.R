library(data.table)
library("plyr")
library(dplyr)
library(arules)
library("xlsx")

datadir <- "/Users/julielee/DDI/"
setwd(datadir)

#idonly <- read.table("test.csv", header = TRUE, sep = "\n")
#tr <- read.transactions("test.csv", format = "basket", sep = ",")

#tem <- sapply(idonly, function(x) strsplit(as.character(x), ",", fixed = TRUE))
#tem <-sapply(tem, function(x) as.numeric(unlist(x)))
#tem <- sapply(strsplit(as.character(idonly), ","), "[", 1)
#names(idonly) <- paste("Tr", c(1:length(idonly)), sep = ",")
tr <- as(tester, "transactions")

association.rules <- apriori(tr, parameter = list(supp = 1/nrow(idonly),minlen=1,maxlen=10,maxtime=5,target="frequent itemsets"))
association.rules.data <- (as(association.rules, 'data.frame'))
fwrite(association.rules.data, file ="association_rules.csv") 
  
outcomes <- as.matrix(fread('FAERS_ADE.txt', header=F, sep='\n'))
#outcomes <- outcomes[1:5000,]
drugid.outcomes <- cbind(idonly, outcomes)

colnames(drugid.outcomes) <- c("drug","outcome")
drugid.outcomes
myopathy <- drugid.outcomes[drugid.outcomes[,2] == 'Myopathy',1]
non.myopathy <- drugid.outcomes[drugid.outcomes[,2] == 'Non_Myopathy',1]

#Run apriori on myopathy for minsupport=1
write.csv(myopathy,"myopathy_only.csv", quote = FALSE, sep = ',', row.names = TRUE)
m.tr <- read.transactions('myopathy_only.csv', format = 'basket', sep=',')

m.association.rules <- apriori(m.tr, parameter = list(supp=1/length(myopathy),minlen=1,maxlen=10,maxtime=5,target="frequent itemsets"))
summary(m.association.rules)
m.association.rules.data <- (as(m.association.rules, 'data.frame'))
fwrite(m.association.rules.data, file ="m_association_rules.csv")

#Merge apriori results for everything and myopathy
everything <- merge(x = m.association.rules.data, y = m.association.rules.data, by = "items", all.x = TRUE, all.y=FALSE)
fwrite(everything, file ="everything.csv")

