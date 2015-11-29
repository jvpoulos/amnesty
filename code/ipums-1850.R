## 1850 complete--count Census. 

# Set data directory
data.directory <- "~/Dropbox/github/amnesty/data/"

# # Import Census data
# ipums <- read.csv(paste0(data.directory,"ipums-1850-all.csv"),header=TRUE, sep = ",")
# 
# # Change column names to lowercase
# names(ipums) <- tolower(names(ipums))
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

# Import sample
ipums.50 <- read.csv(paste0(data.directory,"ipums-1850-sample.csv"),header=TRUE, sep = ",")

# What % in thirteenth exception?
sum(ipums.50$realprop >= 20000) /nrow(ipums.50)
sum(ipums.50$realprop >= 20000)

# Calc property wealth deciles
wealth.dec <- subset(ipums.50, select=c("serial","realprop")) %>%
  mutate(quantile = ntile(realprop, 10))

sum(wealth.dec$realprop[wealth.dec$quantile==10])/sum(wealth.dec$realprop)

head(sort(table(ipums.50$occstr[which(wealth.dec$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth
