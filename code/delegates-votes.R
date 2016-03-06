# Clean and merge southern white delegates with vote scores

require(reshape2)

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

# Split name
names.delegates <- colsplit(delegates$name,",",c("surname","first.name"))

delegates$surname <- trimws(names.delegates$surname) 
delegates$first.name <- trimws(names.delegates$first.name) 

# Make missing age NA
delegates$age[delegates$age==""] <- NA

# Make property values numeric
prop.vars <- c("realprop.60", "persprop.60", "realprop.70", "persprop.70")
delegates$realprop.60 <- gsub("[^[:alnum:]]", "", delegates$realprop.60)
for(var in prop.vars){
  delegates[,var] <- gsub("[^[:alnum:]]", "", delegates[,var])
  delegates[,var] <- as.numeric(delegates[,var])
}

# Make died binary
delegates$died[is.na(delegates$died)] <- 0

# Create taxable property variable
delegates$taxprop.60 <- delegates$realprop.60 + delegates$persprop.60
delegates$taxprop.70 <- delegates$realprop.70 + delegates$persprop.70
