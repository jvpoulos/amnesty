########################
### Delegate votes   ###
########################

# Register cores for parallel processing
registerDoParallel(4)
# Ensure random number generation for parallel processing
RNGkind("L'Ecuyer-CMRG")

# Set location of files
vote.directory <- "data/votes/"

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

# Remove non-alphabetic characters from name and make all uppercase
votes$surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",names.votes$surname))) 
votes$first.name <- trimws(toupper(gsub("[^[:alpha:] ]", "",names.votes$first.name))) 

votes$sound.first <- soundex(votes$first.name) # soundex of first name
votes$sound.surname <- soundex(votes$surname) # soundex of surname name

# Make vote scores numeric
vote.vars <- c("gov","econ","suffrage", "race",  "misc", "overall")
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