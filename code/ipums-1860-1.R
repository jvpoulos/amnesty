## Merge 1860 Census 1% sample with slave schedule.  

# Run 1850 file first
source("ipums-1850.R")

# Import Census data
ipums.60.1 <- read.csv(paste0(data.directory,"ipums-1860-1-sample.csv"),header=TRUE, sep = ",")

# Clean
ipums.60.1 <- CleanIpums1(ipums.60.1)

# Import slave file
slave.60 <- read.csv(paste0(data.directory,"ipums-1860-slave-aux.csv"),header=TRUE, sep = ",")

# Count # slaves by serial
slave.60 <- ddply(slave.60,~serial,summarise,n.slaves=length(serial))

# Merge slave file
ipums.60.1 <- merge(ipums.60.1,slave.60,by="serial", all.x=TRUE)
ipums.60.1$n.slaves[is.na(ipums.60.1$n.slaves)] <- 0

ipums.60.1$slaveholder <- ifelse(ipums.60.1$n.slaves>0,1,0)

# Create taxable property variable
ipums.60.1$taxprop <- ipums.60.1$realprop + ipums.60.1$persprop