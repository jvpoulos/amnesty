## Load 1850 100% Census sample. Merge 1% with slave schedule. 

# Set data directory
data.directory <- "~/Dropbox/github/amnesty/data/"

# # Import Census data
# ipums.50 <- read.csv(paste0(data.directory,"ipums-1850-all.csv"),header=TRUE, sep = ",")
# 
# # Change column names to lowercase
# names(ipums.50) <- tolower(names(ipums.50))
# 
# # Subset to males aged 21 and over who live in south and have nonzero and nonmissing real property 
# fip.south <- c(1,5,12,13,22,28,37,45,47,48,51) # 11 southern states
# 
# ipums.50 <- subset(ipums,  
#                   sex==1 &
#                   statefip %in% icp.south & 
#                   (realprop>0 & realprop!=99999999) & # 99999999 is NIU
#                   (age>=21 & age!= 999))
# 
# # Remove non-alphabetic characters from surname and make all uppercase
# ipums.50$surname<- trimws(toupper(gsub("[^[:alnum:] ]", "",ipums.50$namelast))) 
# 
# # Trim spaces
# ipums.50$surname <- gsub(" ","",ipums.50$surname)
# 
# # Create variable for length of surname
# ipums.50$surname.length <- nchar(ipums.50$surname)
# 
# # Drop obs with missing surnames
# ipums.50 <- subset(ipums.50, ipums.50$surname.length>2)
# 
# write.csv(ipums.50, file = "ipums-1850-sample.csv", row.names = TRUE)

# Import 100% sample
ipums.50 <- read.csv(paste0(data.directory,"ipums-1850-sample.csv"),header=TRUE, sep = ",")

# Import 1% sample
ipums.50.1 <- read.delim(paste0(data.directory,"ipums-1850-1.csv"), quote="")

CleanIpums1 <- function(ipums) {
  # Clean 1% sample
  
  # Subset to individuals with nonzero and nonmissing real property 
  ipums <- subset(ipums, realprop>0)
  
  # Remove non-alphabetic characters from surname and make all uppercase
  ipums$surname<- trimws(toupper(gsub("[^[:alnum:] ]", "",ipums$namelast))) 
  
  # Trim spaces
  ipums$surname <- gsub(" ","",ipums$surname)
  
  # Create variable for length of surname
  ipums$surname.length <- nchar(ipums$surname)
  
  # Drop obs with missing surnames
  ipums <- subset(ipums, ipums$surname.length>2)
}
ipums.50.1 <- CleanIpums1(ipums.50.1)

# Import slave file
slave.50 <- read.csv(paste0(data.directory,"ipums-1850-slave-linked.csv"),header=TRUE, sep = ",")

# Count # slaves by serial
slave.50$is.slave <- ifelse(slave.50$slave=="Slave",1,0)
slave.50 <- ddply(slave.50,~serial,summarise,n.slaves=sum(is.slave))

# Merge slave file
ipums.50.1 <- merge(ipums.50.1,slave.50,by="serial", all.x=TRUE)
ipums.50.1$n.slaves[is.na(ipums.50.1$n.slaves)] <- 0

ipums.50.1$slaveholder <- ifelse(ipums.50.1$n.slaves>0,1,0)

