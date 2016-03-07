# Clean and merge southern white delegates with vote scores

require(reshape2)
require(RecordLinkage)

# Set data directory
data.directory <- "~/Dropbox/github/amnesty/data/"

source("votes.R")

# Import southern white delegates
delegates <- read.csv(paste0(data.directory,"southern-white-delegates.csv"),header=TRUE, sep = ",")

# Remove empty lines
delegates <- subset(delegates, !name=="") 

# Create dummies for footnotes / clean footnotes
delegates$asterisk <- 0
delegates$asterisk[grep("[*]", delegates$name)] <- 1 

delegates$dagger <- 0
delegates$dagger[grep("[+]", delegates$name)] <- 1 

delegates$d.dagger <- 0
delegates$d.dagger[grep("[&]", delegates$name)] <- 1 

delegates$name <- gsub("[^[:alnum:],. ]", "", delegates$name) # remove footnotes
delegates$name <- trimws(delegates$name) # trim name

# Create response var for protest adoption of const. (asterisk: AL,AR, VA; dagger: TX)
delegates$protest <- NA
delegates$protest[delegates$state=="AL" | delegates$state=="AR" | delegates$state=="VA" | delegates$state=="TX"] <- 0
delegates$protest[delegates$asterisk==1 & (delegates$state=="AL" | delegates$state=="AR" | delegates$state=="VA")] <-1 
delegates$protest[delegates$dagger==1 & (delegates$state=="TX")] <-1 

# Split name
names.delegates <- colsplit(delegates$name,",",c("surname","first.name"))

delegates$surname <- trimws(names.delegates$surname) 
delegates$first.name <- trimws(names.delegates$first.name) 

delegates$sound.first <- soundex(delegates$first.name) # soundex of first name
delegates$sound.surname <- soundex(delegates$surname) # soundex of surname

# Make missing age NA
delegates$age[delegates$age==""] <- NA

# Make property values numeric
prop.vars <- c("realprop.60", "persprop.60", "realprop.70", "persprop.70")
for(var in prop.vars){
  delegates[,var] <- gsub("[^[:alnum:]]", "", delegates[,var])
  delegates[,var] <- as.numeric(delegates[,var])
}

# Make died binary
delegates$died[is.na(delegates$died)] <- 0

# Create taxable property variable
delegates$taxprop.60 <- delegates$realprop.60 + delegates$persprop.60
delegates$taxprop.70 <- delegates$realprop.70 + delegates$persprop.70

# Chain 1870 values to 1860 $
delegates$taxprop.70 <- delegates$taxprop.70/(8.06/12.65)
delegates$realprop.70 <- delegates$realprop.70/(8.06/12.65)
delegates$persprop.70 <- delegates$persprop.70/(8.06/12.65)

# Merge delegates with votes
delegates$did <- 1:nrow(delegates) # create unique delegate identifier
votes$vid <- 1:nrow(votes) # create unique votes identifier

r.pairs <- compare.linkage(delegates[c("first.name", "surname", "state", "sound.first","sound.surname")],
                          votes[c("first.name", "surname", "state", "sound.first","sound.surname")])

#min.train <- getMinimalTrain(r.pairs,nEx=10) 
#min.train <- editMatch(min.train)
#saveRDS(min.train, paste0(data.directory,"min_train_delegates.rds"))
min.train <- readRDS(paste0(data.directory,"min_train_delegates.rds"))

model <- trainSupv(min.train, method = "svm")
result <- classifySupv(model, newdata = r.pairs)
summary(result)

links <- result$pairs[result$prediction=="L",][c("id1","id2")]
links <- links[!duplicated(links$id1),] # remove duplicates
                          
delegates <- merge(delegates, links, by.x="did",by.y="id1", all.x=TRUE)
delegates <- merge(delegates, votes, by.x="id2",by.y="vid", all.x=TRUE)
colnames(delegates)[1] <- "vid"

