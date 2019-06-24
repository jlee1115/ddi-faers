#install.packages("data.table", dependencies=TRUE)
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("arules")
#install.packages("jsonlite")
#install.packages("xlsx")


library(data.table)
library("plyr")
library(dplyr)
library(arules)
library("xlsx")

datadir <- "/Users/julielee/DDI/"
setwd(datadir)

#Read in Drug Bank data and remove duplicates
atcdata <- read.xlsx("/Users/julielee/DDI/ATC_DrugBank_approved.xlsx", 1, header=TRUE)
atcsorted <- atcdata[order(atcdata$ATC.code),]
atcsorted$Name<-sapply(atcsorted$Name, tolower)
atcsorted <- atcsorted[!duplicated(atcsorted$Name),]
write.csv(atcsorted, 'atcapproved.csv')


#Create new DrugBank Table with Group IDs and Group Description
drugName <- atcsorted$Name
ATC <- atcsorted$ATC.code 
atctable <- data.frame(drugName, ATC)
preGroupID <- ifelse(atcsorted$ATC.code == 'none', 
                     'none', paste0(substr(atcsorted$ATC.code, 0, 1), '*'))
GroupID <- ifelse(atcsorted$ATC.code == 'none', 
                  'none', substr(atcsorted$ATC.code, 0, 1))
GroupID <- ifelse(atcsorted$ATC.code == 'none', 
                  0, 10000 + as.numeric(factor(GroupID)))

GroupDescription <- paste0('Code Group Form is of type: ', preGroupID)
atctable <- cbind(atctable, GroupID, GroupDescription)
matchlist = c(0)
x <- as.numeric(atctable$GroupID) %in% matchlist
atctable <- rbind(atctable[!x,], atctable[x,])
write.csv(atctable, 'atctable.csv')


#Create Group ID Table that describes what each GroupID indicates
groupidtable <- select(atctable [!duplicated(atctable[c(3,4)]),], GroupID, GroupDescription)
groupidtable <- groupidtable[!(groupidtable$GroupID == 0),]
drugid <- read.delim("drugid_code.txt")
drugid$drugName<-sapply(drugid$drugName, tolower)


#Go through everything in Tiffany's druglist
#Make sure our list of drugs is in the Drug Bank

#Overlapping drugs
overlapyes <- select(drugid[which(drugid$drugName %in% atctable$drugName), ], drugId, drugName)
overlap <- select(atctable[which(atctable$drugName %in% drugid$drugName), ], drugName, GroupID)
tempsorted <- overlap[order(overlap$drugName),]
overlap <- merge(overlapyes, tempsorted)
overlap <- overlap[order(as.numeric(overlap$GroupID)),]
nocode <- overlap[overlap$GroupID < 10000, ]
overlap <- rbind(overlap[overlap$GroupID >= 10000, ], nocode)

overlap$GroupID <- ifelse(overlap$GroupID == 0, overlap$drugId, overlap$GroupID)
GroupID <- overlap$GroupID[overlap$GroupID < 10000]
GroupDescription <- rep("No ATC Code",length(GroupID))
indi <- cbind(GroupID, GroupDescription)
groupidtable <- rbind(groupidtable, indi)
write.csv(overlap, 'overlap.csv')


#Nonoverlapping drugs
overlapno <- select(drugid[which(!drugid$drugName %in% atctable$drugName), ], drugId, drugName)
overlapno$GroupID <- overlapno$drugId

#Combine overlapping and nonoverlapping Drugs. Update GroupID table
GroupID <- overlapno$GroupID
GroupDescription <- rep("Individual Drug not in DB",length(nrow(overlapno)))
indi <- cbind(GroupID, GroupDescription)
groupidtable <- rbind(groupidtable, indi)
write.csv(groupidtable, 'groupidtable.csv')

#Final table containing Drug ID, Drug name, Group ID 
completetable <- rbind(overlap, overlapno)
completetable$drugName <-sapply(completetable$drugName, toupper)
write.csv(completetable, 'completetable.csv')

#Relabel FAERS data with new Group IDs
#Relabel FAERS data with new Group IDs
drug <- as.matrix(fread('FAERS_Drug.txt', header=F, sep='\n'))
drug.l <- unlist(strsplit(drug, split='\\$___\\$'))
drug.t <- table(drug.l)
print("drug.t")
drug.t
drug.t1 <- as.matrix(drug.t)
print("drug.t1")
drug.t1

dt1 = as.data.table(drug)
colnames(dt1)[colnames(dt1)=="V1"] <- "names"
print("dt1")
dt1

id <- rownames(dt1)
dt1 <- cbind(id=id, dt1)
dt1
drug.t2 <- cbind(completetable$drugName, completetable$GroupID)
colnames(drug.t2) <- c('drugName', 'GroupID')
dt2 <- as.data.table(drug.t2)

setkey(dt2, drugName)
dt2

dt2 <- dt2[setkey(dt1[, strsplit(as.character(names), split = "\\$___\\$"), by = id], V1)]
dt2
dt2$id <- as.numeric(as.character(dt2$id))
dt2 <- dt2[,lapply(.SD, paste0, collapse = ","), keyby = id]
dt2
dt2.id <- select(dt2,GroupID)
write.table(dt2.id, 'drugid_only.txt', row.names=F, quote=F, col.names=T, sep='\t')

maxi <- max(count.fields('drugid_only.txt', sep = ','))
idonly <- read.csv('drugid_only.txt', header = FALSE, sep = ",", col.names = paste0("V",seq_len(maxi)), fill = TRUE)
idonly <- apply(idonly, 1, function(x) unique(x))
idonly <- plyr::ldply(idonly, rbind)
write.table(idonly, 'drugidclean.txt', row.names=F, quote=F, col.names=T, sep='\t')
 



