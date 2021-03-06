##########################################
### 1860 Slaveholder Census           ###
##########################################

train <- FALSE

alabama <- read_excel("data/slaveholders_1860/Alabama.xlsx")
georgia <- read_excel("data/slaveholders_1860/Georgia.xlsx")
mississippi <- read_excel("data/slaveholders_1860/Mississippi.xlsx")
south.carolina <- read_excel("data/slaveholders_1860/South Carolina.xlsx")

alabama$pid <- as.numeric(alabama$pid)
georgia$pid <- as.numeric(georgia$pid)
georgia$`Real Estate 1870` <- as.numeric(georgia$`Real Estate 1870`)
mississippi$`Real Estate` <- as.numeric(mississippi$`Real Estate`)
south.carolina$`Real Estate` <- as.numeric(south.carolina$`Real Estate`)
south.carolina$`Personal Estate` <- as.numeric(south.carolina$`Personal Estate`)

slaveholders.60 <- bind_rows(alabama,georgia,mississippi,south.carolina)

# State and county names
icpsrcnt <- read.delim("data/icpsrcnt.txt", comment.char="#",
                       col.names=c('state',	'stateicp',	'statefips'	,'county.code',	'county.name'))

slaveholders.60 <- merge(slaveholders.60, icpsrcnt, 
                  by.x = c("self_residence_place_state", "self_residence_place_county"), 
                  by.y = c("state","county.name"),
                  all.x = TRUE)

# Create common colnames
colnames(slaveholders.60)[1] <- "state"
colnames(slaveholders.60)[2] <- "county"

# Remove non-alphabetic characters from name and make all uppercase
slaveholders.60$surname <- trimws(toupper(gsub("[^[:alpha:] ]", "",slaveholders.60$self_empty_name_surname))) 
slaveholders.60$first <- trimws(toupper(gsub("[^[:alpha:] ]", "",slaveholders.60$self_empty_name_given))) 

slaveholders.60 <- CleanIpums(slaveholders.60,complete=TRUE)

# Remove female slaveholders
female.names.1860 <- read.csv("data/female-names-1860.csv")

slaveholders.60$female <- ifelse(slaveholders.60$first %in% female.names.1860$x,1,0)

slaveholders.60 <- subset(slaveholders.60, female!=1)

# Wealth NA is 0 for 1860, 1870 if successful link
slaveholders.60$`Real Estate`[is.na(slaveholders.60$`Real Estate`)] <- 0
slaveholders.60$`Personal Estate`[is.na(slaveholders.60$`Personal Estate`)] <- 0

slaveholders.60$`Real Estate 1870`[is.na(slaveholders.60$`Real Estate 1870`) & slaveholders.60$pid.1870!="NA"] <- 0
slaveholders.60$`Personal Estate 1870`[is.na(slaveholders.60$`Personal Estate 1870`) & slaveholders.60$pid.1870!="NA"] <- 0

slaveholders.60$taxprop.1870 <- slaveholders.60$`Real Estate 1870` +  slaveholders.60$`Personal Estate 1870`

slaveholders.60$taxprop <- slaveholders.60$`Real Estate` +  slaveholders.60$`Personal Estate`
#slaveholders.60 <- slaveholders.60[slaveholders.60$taxprop>0,] # keep positive wealth
#slaveholders.60 <- slaveholders.60[slaveholders.60$taxprop.1870>0 | is.na(slaveholders.60$taxprop.1870),] # keep positive wealth

# Chain 1870 values to 1860 $
slaveholders.60$taxprop.1870 <- slaveholders.60$taxprop.1870/(8.06/12.65)
slaveholders.60$`Real Estate 1870` <- slaveholders.60$`Real Estate 1870`/(8.06/12.65)
slaveholders.60$`Personal Estate 1870` <- slaveholders.60$`Personal Estate 1870`/(8.06/12.65)

# Create measure of change in wealth between Censuses
slaveholders.60$taxprop.d <- slaveholders.60$taxprop.1870-slaveholders.60$taxprop
slaveholders.60$realprop.d <- slaveholders.60$`Real Estate 1870` - slaveholders.60$`Real Estate`
slaveholders.60$persprop.d <- slaveholders.60$`Personal Estate 1870` - slaveholders.60$`Personal Estate`

# Create profession dummies
slaveholders.60$Occupation<- trimws(gsub("[^[:alnum:] ]", "", slaveholders.60$Occupation))

slaveholders.60$farmer <- 0
slaveholders.60[c(grep("Farm*",slaveholders.60$Occupation),
                  grep("Fam*",slaveholders.60$Occupation),
                  grep("Plant*",slaveholders.60$Occupation),
                  grep("Over*",slaveholders.60$Occupation)),]$farmer <- 1

slaveholders.60$lawyer <- 0
slaveholders.60[c(grep("Att*",slaveholders.60$Occupation),
                  grep("Marshal",slaveholders.60$Occupation),
                  grep("Judge",slaveholders.60$Occupation),
                  grep("Sheriff",slaveholders.60$Occupation),
                  grep("Offic*",slaveholders.60$Occupation),
                  grep("Administrator",slaveholders.60$Occupation),
                  grep("Mayor",slaveholders.60$Occupation),
                  grep("Lawyer",slaveholders.60$Occupation),
                  grep("Police*",slaveholders.60$Occupation)),]$lawyer <- 1 # lawyer/law Professional

slaveholders.60$merchant <- 0
slaveholders.60[c(grep("Bank*",slaveholders.60$Occupation),
                  grep("Merc*",slaveholders.60$Occupation),
                  grep("Broker",slaveholders.60$Occupation),
                  grep("Dealer",slaveholders.60$Occupation),
                  grep("Trader",slaveholders.60$Occupation),
                  grep("Auction*",slaveholders.60$Occupation)),]$merchant <- 1 # merchant/banker/accountant

slaveholders.60$physician <- 0
slaveholders.60[c(grep("Doctor",slaveholders.60$Occupation),
                  grep("Dr*",slaveholders.60$Occupation),
                  grep("MD",slaveholders.60$Occupation),
                  grep("Phys*",slaveholders.60$Occupation),
                  grep("Dentist",slaveholders.60$Occupation)),]$physician <-1 #physician/dentist/druggist

slaveholders.60$minister <- 0
slaveholders.60[c(grep("Bapt*",slaveholders.60$Occupation),
                  grep("Clergy*",slaveholders.60$Occupation),
                  grep("Mini*",slaveholders.60$Occupation),
                  grep("Methodist",slaveholders.60$Occupation),
                  grep("Preacher",slaveholders.60$Occupation)),]$minister <- 1 

slaveholders.60$other <-0
slaveholders.60[which(rowSums(slaveholders.60[c("farmer","lawyer","merchant","physician","minister")])== 0),]$other <-1 

## Merge slaveholders with 1860 full-count records
unzip("data/ipums-1860-100-sample.csv.zip",exdir="data/")

ipums.1860.100.sample <- read.csv("data/ipums-1860-100-sample.csv")

slaveholders.60 <- merge(slaveholders.60, ipums.1860.100.sample[c("pid",setdiff(colnames(ipums.1860.100.sample),colnames(slaveholders.60)))], by="pid",all.x=TRUE)

## Merge slaveholders with pardons

slaveholders.60$state.abb <- state.abb[match(slaveholders.60$state,state.name)]

# Merge by soundex surname and state
pardons.slaveholders.match <- merge(slaveholders.60,pardons,by.x=c("sound.surname","state.abb"),by.y=c("sound.surname","state")) 

#resort by pid
pardons.slaveholders.match <- pardons.slaveholders.match[order(pardons.slaveholders.match$pid.x),]

# create match id
pardons.slaveholders.match$match.id <- 1:nrow(pardons.slaveholders.match)

# Create Jaro similarity measure on first name and surname
pardons.slaveholders.match$jaro.surname <- jarowinkler(pardons.slaveholders.match$surname.x,pardons.slaveholders.match$surname.y) 
pardons.slaveholders.match$jaro.first <- jarowinkler(pardons.slaveholders.match$first,pardons.slaveholders.match$first.name)

# Create exact match variables
pardons.slaveholders.match$exact.surname <- ifelse(pardons.slaveholders.match$surname.x==pardons.slaveholders.match$surname.y,1,0)
pardons.slaveholders.match$exact.first.name <- ifelse(pardons.slaveholders.match$first==pardons.slaveholders.match$first.name,1,0)

# Scale and center continuous vars
preProcValues <- preProcess(pardons.slaveholders.match[c("jaro.surname","jaro.first")], method = c("center", "scale")) 
pardons.slaveholders.match[c("jaro.surname","jaro.first")] <- predict(preProcValues, pardons.slaveholders.match[c("jaro.surname","jaro.first")])

# Split 1/3 
bound <- floor((nrow(pardons.slaveholders.match))*0.1)         #define % of training and test set

set.seed(42) # set seed for reproducibility
df <- pardons.slaveholders.match[sample(nrow(pardons.slaveholders.match)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.match <- 0
train.matches <- c(7164,5230,7266,6832,6515,106,7147,9436,9803,8973,2456,5683,9074,6169,1437,6900,9345,422,3510,6976,3162,664,7714,351,5934,851,9373,9974,
                   1350,118,7582,7974,8463,683,3781,5831,7427,1422,7152,793,3800,6695,4781) #match id
df.train$is.match[df.train$match.id %in% train.matches] <-1

# Create features vector
X.train <-df.train[c("jaro.surname","jaro.first","exact.surname","exact.first.name")]
X.test <-df.test[c("jaro.surname","jaro.first","exact.surname","exact.first.name")]

jaro.first.mode <- Mode(X.train$jaro.first) # impute missing data with training mode
exact.first.mode <- Mode(X.train$exact.first.name)
X.train$jaro.first[is.na(X.train$jaro.first)] <- jaro.first.mode
X.train$exact.first.name[is.na(X.train$exact.first)] <- exact.first.mode

X.test$jaro.first[is.na(X.test$jaro.first)] <- jaro.first.mode
X.test$exact.first.name[is.na(X.test$exact.first)] <- exact.first.mode

# Create outcomes vector
Y.train <- as.matrix(df.train$is.match)

if(train){
  # Train 
  set.seed(42)
  fitSL.link <- SuperLearner(Y=Y.train[,1],
                             X=data.frame(X.train),
                             SL.library=SL.library.class,
                             family="binomial") # glmnet response is 2-level factor
  
  # Save prediciton model
  saveRDS(fitSL.link, file = "data/slaveholders_pardon_link.rds")
}else{
  fitSL.link <- readRDS("data/slaveholders_pardon_link.rds")
}

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$match.prob <- as.numeric(link.pred.test) # match probability 
X.test$match <- ifelse(X.test$match.prob>0.5,1,0) 

# Merge training, test matches to slaveholders.60
train.matches.pid <- df.train$pid.x[df.train$is.match==1]
test.matches.pid <- unique(df.test$pid.x[X.test$match==1]) # duplicates

slaveholders.60$pardon <- 0
slaveholders.60$pardon[slaveholders.60$pid %in% c(train.matches.pid, test.matches.pid)] <- 1

## Preprocess matched data

# Subset data to nonmissing taxable property values
slaveholders.60.rd <- subset(slaveholders.60, !is.na(taxprop))

# RD parameters
cutoff <- 20000 # define cutoff
upper <- 2*cutoff # define upper margin

# Treatment status variable
slaveholders.60.rd$treat <- ifelse(slaveholders.60.rd$taxprop>=cutoff,1,0)

slaveholders.60.rd$tot <- slaveholders.60.rd$treat
slaveholders.60.rd$tot[slaveholders.60.rd$pardon==1] <-0

# Compliance rate
slaveholders.60.beta <- sum(slaveholders.60.rd$tot)/sum(slaveholders.60.rd$treat)