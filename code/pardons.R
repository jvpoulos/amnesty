########################
### Pardon grants    ###
########################

# Import data
pardons.99 <- read.csv(paste0(data.directory,"pardons-exec-doc-99.csv"), header=TRUE, stringsAsFactors=FALSE)

# Row-bind columns
pardons <- data.frame("name"=c(pardons.99$name1,pardons.99$name2,pardons.99$name3),
                      "state"=rep(pardons.99$state, by=ncol(pardons.99)))

pardons <- pardons[pardons$name!="",]

# Split name
names.pardons <- colsplit(pardons$name,",",c("surname","first.name"))

pardons$surname <- trimws(names.pardons$surname) 
pardons$first.name <- trimws(names.pardons$first.name) 

# Remove non-alphabetic characters from name and make all uppercase
pardons$surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",pardons$surname))) 
pardons$first.name <- trimws(toupper(gsub("[^[:alpha:] ]", "",pardons$first.name))) 

# Trim spaces in name
pardons$surname <- gsub(" ","",pardons$surname)
pardons$first.name <- gsub("  ", " ",pardons$first.name)

# Split first and middle name
pardons$first.name <- trimws(unlist(lapply(strsplit(pardons$first.name," "), function(x) x[1])))

# Drop obs with missing names
pardons$surname.length <- nchar(pardons$surname)
pardons$first.length <- nchar(pardons$first.name)
pardons <- subset(pardons, surname.length>2 & first.length>0)

# Standardize first name
pardons$first.name <- StFirst(pardons$first.name)

# Create soundex of first and surnames
pardons$sound.surname <- soundex(pardons$surname)
pardons$sound.first <- soundex(pardons$first.name)

rm(names.pardons, pardons.99) # clean space