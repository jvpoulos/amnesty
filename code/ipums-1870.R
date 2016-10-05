#####################
### 1870 Census   ###
#####################

## 100% Sample
# Load 1870 100% sample and clean
unzip(paste0(data.directory, "ipums-1870-100-sample.csv.zip"), exdir=data.directory) # unzip sample
ipums.70 <- read.csv(paste0(data.directory,"ipums-1870-100-sample.csv"),header=TRUE, sep = ",") 

# Clean
ipums.70 <- CleanIpums(ipums.70,complete = TRUE)