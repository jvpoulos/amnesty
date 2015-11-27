## 1850 complete--count Census. 

library(ggplot2)
library(xtable)

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
# ipums.s <- subset(ipums,  
#                   sex==1 &
#                   statefip %in% icp.south & 
#                   (realprop>0 & realprop!=99999999) & # 99999999 is NIU
#                   (age>=21 & age!= 999))
# 
# # Remove non-alphabetic characters from surname and make all uppercase
# ipums.s$surname<- trimws(toupper(gsub("[^[:alnum:] ]", "",ipums.s$namelast))) 
# 
# # Trim spaces
# ipums.s$surname <- gsub(" ","",ipums.s$surname)
# 
# # Create variable for length of surname
# ipums.s$surname.length <- nchar(ipums.s$surname)
# 
# # Drop obs with missing surnames
# ipums.s <- subset(ipums.s, ipums.s$surname.length>2)
# 
# write.csv(ipums.s, file = "ipums-1850-sample.csv", row.names = TRUE)

# Import sample
ipums.s <- read.csv(paste0(data.directory,"ipums-1850-sample.csv"),header=TRUE, sep = ",")

# What % in thirteenth exception?
sum(ipums.s$realprop >= 20000) /nrow(ipums.s)
sum(ipums.s$realprop >= 20000)

# Make density plot for real property
wealth.dens <- ggplot(ipums.s, aes(x = realprop)) + 
  geom_density(aes(y=..count../sum(..count..))) + 
  xlim(c(0,30000)) +
  ylab("Proportion") +
  xlab("Value of real property owned") +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash")

ggsave(paste0(data.directory,"wealth-dens.png"), wealth.dens, width=11, height=8.5)

# Create summary statistics table.
ipums.s$literate <- ifelse(ipums.s$LIT==4,1,0) # create variables
ipums.s$school <- ifelse(ipums.s$SCHOOL==2,1,0)
ipums.s <- cbind(ipums.s,dummify(as.factor(ipums.s$OCC)))

my.stats <- list("n", "min", "mean", "max", "s") # create table
tableContinuous(vars =ipums.s[c("surname.length","surname.freq","AGE","REALPROP","literate","school","5","22","39","41","49","54","97","136","157","203","266")], prec = 3,stats=my.stats,cap = "`Surname length' is the character length of surnames. `Surname frequency' is the number of times surnames appear in the sample. `Literate' is a binary variable indicating literacy (can read and write). `In school' is an indicator variable for individuals currently in school. Sample is drawn from the 1850 full--count Census. The occupations dummies indicate contemporary occupational categories. Sample is restricted to male heads of households aged 21 and over who living in Georgia at the time of the census, were born in Georgia, and have non--missing surnames and property value.", lab = "sum-1850")

# Calc property wealth deciles
wealth.dec <- subset(ipums.s, select=c("SERIAL","REALPROP")) %>%
  mutate(quantile = ntile(REALPROP, 10))

sum(wealth.dec$REALPROP[wealth.dec$quantile==10])/sum(wealth.dec$REALPROP)

head(sort(table(ipums.s$OCCSTR[which(wealth.dec$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth
head(sort(table(ipums.s$COUNTY[which(wealth.dec$quantile==10)]), decreasing = TRUE)) # see which counties are in top decile of wealth
