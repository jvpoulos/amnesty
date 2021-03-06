#####################
### 1860 Census   ###
#####################

## 100% Sample
# Load 1860 100% sample
unzip("data/ipums-1860-100-sample.csv.zip", exdir="data/") # unzip sample
ipums.60 <- read.csv("data/ipums-1860-100-sample.csv",header=TRUE, sep = ",") 

# Clean
ipums.60 <- CleanIpums(ipums.60, complete = TRUE)

## Slavepums

# Load flat and aux slave files
unzip("data/slavepums-60-flat.csv.zip", exdir="data/") # unzip sample
unzip("data/slavepums-60-aux.csv.zip", exdir="data/") # unzip sample

slave.60.flat <- read.csv("data/slavepums-60-flat.csv",header=TRUE, sep = ",")
slave.60.aux <- read.csv("data/slavepums-60-aux.csv",header=TRUE, sep = ",")

# Link by serial
slave.60 <- merge(slave.60.flat, slave.60.aux, by=c("serial"), all.x=TRUE)

# State and county names
icpsrcnt <- read.delim("data/icpsrcnt.txt", comment.char="#",
                       col.names=c('state',	'stateicp',	'statefips'	,'county.code',	'county.name'))

slave.60 <- merge(slave.60, icpsrcnt, 
                  by.x = c("statefip", "county"), 
                  by.y = c("statefips","county.code"),
                  all.x = TRUE)

# Create common colnames
colnames(slave.60)[2] <- "county.code"
colnames(slave.60)[100] <- "county"

# Keep states in Confederacy 
slave.60 <- subset(slave.60, state!="District of Columbia" & 
                     state!="Delaware" &
                     state!="Missouri" &
                     state!="Maryland" &
                     state!="Kentucky" & 
                     state!="Utah" & 
                     state!="West Virginia")

# Remove non-alphabetic characters from name and make all uppercase
slave.60$surname <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1ln))) 
slave.60$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",slave.60$sh1fn))) 

slave.60 <- CleanIpums(slave.60,complete=FALSE)

# Remove female slaveholders
female.names.1860 <- read.csv("data/female-names-1860.csv")

slave.60$female <- ifelse(slave.60$first %in% female.names.1860$x,1,0)

slave.60 <- subset(slave.60, female!=1)

# Create unique id by name, state
slave.60 <- transform(slave.60,id=as.numeric(factor(paste0(slave.60$first, " ", slave.60$surname, ", ", slave.60$state, ", ", slave.60$county.name))))

# Count # slaves by unique id
n.slaves <- ddply(slave.60,~id,summarise,n.slaves=length(id))
slave.60 <- merge(slave.60,n.slaves, by="id") # merge back slave counts

# Aggregate by id and remove slave-level variables
slave.60 <- subset(slave.60[!duplicated(slave.60$id),], select=c("id","surname","first","middle.name","surname.length",
                                                                 "first.length","first.initial","sound.surname","sound.first","n.slaves","state","county"))

colnames(slave.60) <- c("slavepums.id","surname","first","middle.name","surname.length",
                        "first.length","first.initial","sound.surname","sound.first","n.slaves","state","county")

## Counties
census.county.1860 <- read.csv("data/census-county-1860.csv", stringsAsFactors=FALSE)
icpsrcnt <- read.delim("data/icpsrcnt.txt", comment.char="#")

census.county.1860 <- merge(census.county.1860,icpsrcnt, by.x =c("state","county"), by.y = c("STATEICP","County.cod"))
