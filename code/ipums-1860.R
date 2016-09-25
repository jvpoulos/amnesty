#####################
### 1860 Census   ###
#####################

## 100% Sample
# Load 1860 100% sample and clean
unzip(paste0(data.directory, "ipums-1860-100-sample.csv.zip"), exdir=data.directory) # unzip sample
ipums.60 <- read.csv(paste0(data.directory,"ipums-1860-100-sample.csv"),header=TRUE, sep = ",") 

ipums.60 <- CleanIpums(ipums.60,one.perc = FALSE)

# Create common colnames
ipums.60$county <- ipums.60$general_county_orig

## 1% Sample
# Load 1860 1% sample and clean
ipums.60.1 <- read.csv(paste0(data.directory,"ipums-1860-1-sample.csv"),header=TRUE, sep = ",")

ipums.60.1 <- CleanIpums(ipums.60.1)

ipums.60.1$taxprop <- ipums.60.1$realprop + ipums.60.1$persprop # create taxable property variable

## Slavepums

# Load flat and aux slave files
unzip(paste0(data.directory, "slavepums-60-flat.csv.zip"), exdir=data.directory) # unzip sample
unzip(paste0(data.directory, "slavepums-60-aux.csv.zip"), exdir=data.directory) # unzip sample

slave.60.flat <- read.csv(paste0(data.directory,"slavepums-60-flat.csv"),header=TRUE, sep = ",")
slave.60.aux <- read.csv(paste0(data.directory,"slavepums-60-aux.csv"),header=TRUE, sep = ",")

# Link by serial
slave.60 <- merge(slave.60.flat, slave.60.aux, by=c("serial"), all.x=TRUE)

# State and county names
icpsrcnt <- read.delim("~/Dropbox (Personal)/github/amnesty/data/icpsrcnt.txt", comment.char="#",
                       col.names=c('state',	'stateicp',	'statefips'	,'county.code',	'county.name'))

slave.60 <- merge(slave.60, icpsrcnt, 
                  by.x = c("statefip", "county"), 
                  by.y = c("statefips","county.code"),
                  all.x = TRUE)

# Create common colnames
colnames(slave.60)[2] <- "county.code"
colnames(slave.60)[107] <- "county"

# Keep states in Confederacy (missing NC)
slave.60 <- subset(slave.60, state!="District of Columbia" & 
                     state!="Kentucky" & 
                     state!="Utah" & 
                     state!="West Virginia")

# Remove non-alphabetic characters from name and make all uppercase
slave.60$surname <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1ln))) 
slave.60$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1fn))) 

slave.60 <- CleanIpums(slave.60,one.perc=FALSE)

# Create unique id by name, state
slave.60 <- transform(slave.60,id=as.numeric(factor(paste0(slave.60$first, " ", slave.60$surname, ", ", slave.60$state, ", ", slave.60$county))))

# Count # slaves by unique id
n.slaves <- ddply(slave.60,~id,summarise,n.slaves=length(id))
slave.60 <- merge(slave.60,n.slaves, by="id") # merge back slave counts

# Aggregate by id and remove slave-level variables
slave.60 <- subset(slave.60[!duplicated(slave.60$id),], select=c("id","surname","first","middle.name","surname.length",
                                                                 "first.length","sound.surname","sound.first","n.slaves","state","county"))

# Estimate avg. values of slaves in 1860
slave.60$slave.value <- slave.60$n.slaves * 778
