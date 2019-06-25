library(data.table)
library("plyr")
library(dplyr)
library(arules)
library("xlsx")

datadir <- "/Users/julielee/DDI/"
setwd(datadir)


idonly <- read.table("drugidclean.txt", header = TRUE, sep = "\t")
#idonly<- as.matrix(fread('drugidclean.txt', header=TRUE, sep='\t'))
#idonly <- data.frame(idonly)
#idonly <- apply(idonly[-1], 1, function(x) unique(x[!is.na(x)]))
#idonly
#idonly <- read.csv('drugidclean.txt', header = FALSE, sep = ",", col.names = paste0)

#write.csv(idonly, "transactions.csv", quote = FALSE, row.names = TRUE)
#tr <- read.transactions('transactions.csv', format = 'basket', sep = ',')
association.rules <- apriori(tr, parameter = list(supp=1000/nrow(idonly),minlen=1,maxlen=10,maxtime=5,target="frequent itemsets"))
association.rules.data <- (as(association.rules, 'data.frame'))
fwrite(association.rules.data, file ="association_rules.csv")

outcomes <- as.matrix(fread('FAERS_ADE.txt', header=F, sep='\n'))
drugid.outcomes <- cbind(idonly$GroupID, outcomes)
idonly
idonly$GroupID
ncol(drugid.outcomes)
colnames(drugid.outcomes) <- c("drug","outcome")
myopathy <- drugid.outcomes[drugid.outcomes[,2] == 'Myopathy',1]
non.myopathy <- drugid.outcomes[drugid.outcomes[,2] == 'Non_Myopathy',1]

#Run apriori on myopathy for minsupport=1
write.csv(myopathy,"myopathy_only.csv", quote = FALSE, row.names = TRUE)
m.tr <- read.transactions('myopathy_only.csv', format = 'basket', sep=',')
m.association.rules <- apriori(m.tr, parameter = list(supp=1/length(myopathy),minlen=1,maxlen=10,maxtime=5,target="frequent itemsets"))
summary(m.association.rules)
m.association.rules.data <- (as(m.association.rules, 'data.frame'))
fwrite(m.association.rules.data, file ="m_association_rules.csv")

#Merge apriori results for everything and myopathy
everything <- merge(x = association.rules.data, y = m.association.rules.data, by = "items", all.x = TRUE, all.y=FALSE)
fwrite(everything, file ="everything.csv")

