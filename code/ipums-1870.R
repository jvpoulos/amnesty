#####################
### 1870 Census   ###
#####################

## 100% Sample
# Load 1870 100% sample and clean
unzip("data/ipums-1870-100-sample.csv.zip", exdir="data/") # unzip sample
ipums.70 <- read.csv("data/ipums-1870-100-sample.csv",header=TRUE, sep = ",") 

# Clean
ipums.70 <- CleanIpums(ipums.70,complete = TRUE)