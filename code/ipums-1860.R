#####################
### 1860 Census   ###
#####################

# Load 1860 100% sample
unzip(paste0(data.directory, "ipums-1860-100-sample.csv.zip"), exdir=data.directory) # unzip sample
ipums.60 <- read.csv(paste0(data.directory,"ipums-1860-100-sample.csv"),header=TRUE, sep = ",") 

# Load 1860 1% sample and clean
ipums.60.1 <- read.csv(paste0(data.directory,"ipums-1860-1-sample.csv"),header=TRUE, sep = ",")

ipums.60.1 <- CleanIpums(ipums.60.1)

ipums.60.1$taxprop <- ipums.60.1$realprop + ipums.60.1$persprop # create taxable property variable

# Load complete count slave file 
unzip(paste0(data.directory, "slavepums-1860-complete.csv.zip"), exdir=data.directory) # unzip sample
slave.60 <- read.csv(paste0(data.directory,"slavepums-1860-complete.csv"),header=TRUE, sep = ",")

# Remove non-alphabetic characters from name and make all uppercase
slave.60$surname <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1ln))) 
slave.60$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1fn))) 

slave.60 <- CleanIpums(slave.60,one.perc=FALSE)

# Keep states in Confederacy (missing NC)
slave.60 <- subset(slave.60, statefip!="District of Columbia" & 
                     statefip!="Kentucky" & 
                     statefip!="Utah" & 
                     statefip!="West Virginia")

# Create unique id by name, state
slave.60 <- transform(slave.60,id=as.numeric(factor(paste0(slave.60$first, " ", slave.60$surname, ", ", slave.60$state))))

# Count # slaves by unique id
n.slaves <- ddply(slave.60,~id,summarise,n.slaves=length(id))
slave.60 <- merge(slave.60,n.slaves, by="id") # merge back slave counts

# Create common colnames
slave.60$state <- slave.60$statefip

# Aggregate by id and remove slave-level variables
slave.60 <- subset(slave.60[!duplicated(slave.60$id),], select=c("id","county","surname","first","middle.name","surname.length",
                                                                 "first.length","sound.surname","sound.first","n.slaves","state"))

# Estimate avg. values of slaves in 1860
slave.60$slave.value <- slave.60$n.slaves * 778
