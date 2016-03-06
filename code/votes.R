# Combine and clean delegate vote scores

require(doParallel)
require(data.table)
require(RecordLinkage)

# Register cores for parallel processing
registerDoParallel(4)
# Ensure random number generation for parallel processing
RNGkind("L'Ecuyer-CMRG")

# Set location of files
vote.directory <- paste0(data.directory, "votes/", sep="")

# Import vote files and append data row-wise
vote.files <- c("AL-votes.csv","AR-votes.csv", "GA-votes.csv", "LA-votes.csv", "MS-votes.csv", "NC-votes.csv", "TX-votes.csv", "VA-votes.csv")

ImportData <- function(directory,files) {
  data <- foreach(i = files, .combine = rbind) %dopar%{
    cbind(read.csv(paste0(directory,i)))
  }
  return(data)
}

votes <- ImportData(vote.directory,vote.files)

# Split name
names.votes <- colsplit(votes$name,",",c("surname","first.name"))

votes$surname <- trimws(names.votes$surname) 
votes$first.name <- trimws(names.votes$first.name) 

votes$sound.first <- soundex(votes$first.name) # soundex of first name
votes$sound.surname <- soundex(votes$surname) # soundex of surname name

# Make vote scores numeric
vote.vars <- c("gov","suffrage", "race",  "misc", "overall")
for(var in vote.vars){
  votes[,var] <- gsub("[^[:alnum:].]", "", votes[,var])
  votes[,var] <- as.numeric(votes[,var])
}

# Restrict to southern whites
votes$bloc <- gsub("0. White", "Outside White", votes$bloc)
votes$bloc <- gsub("O.White", "Outside White", votes$bloc)
votes$bloc <- gsub("O.Wbite", "Outside White", votes$bloc)
votes$bloc <- gsub("s. White", "Southern White", votes$bloc)
votes$bloc <- gsub("S. White", "Southern White", votes$bloc)
votes$bloc <- gsub("S.White", "Southern White", votes$bloc)
votes$bloc[votes$bloc==""] <- NA

votes <- votes[grep("Southern White",votes$bloc),]


