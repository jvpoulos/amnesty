##############################################################
### Merge southern white delegates with votes and pardons   ###
##############################################################

# Import southern white delegates
delegates <- read.csv(paste0(data.directory,"southern-white-delegates.csv"),header=TRUE, sep = ",")

# Remove empty lines
delegates <- subset(delegates, !name=="") 

# Create dummies for footnotes / clean footnotes
delegates$asterisk <- 0
delegates$asterisk[grep("[*]", delegates$name)] <- 1 

delegates$dagger <- 0
delegates$dagger[grep("[+]", delegates$name)] <- 1 

delegates$d.dagger <- 0
delegates$d.dagger[grep("[&]", delegates$name)] <- 1 

delegates$name <- gsub("[^[:alnum:],. ]", "", delegates$name) # remove footnotes
delegates$name <- trimws(delegates$name) # trim name

# Create response var for protest adoption of const. (asterisk: AL,AR, VA; dagger: TX)
delegates$protest <- NA
delegates$protest[delegates$state=="AL" | delegates$state=="AR" | delegates$state=="VA" | delegates$state=="TX"] <- 0
delegates$protest[delegates$asterisk==1 & (delegates$state=="AL" | delegates$state=="AR" | delegates$state=="VA")] <-1 
delegates$protest[delegates$dagger==1 & (delegates$state=="TX")] <-1 

# Split name
names.delegates <- colsplit(delegates$name,",",c("surname","first.name"))

# Remove non-alphabetic characters from name and make all uppercase
delegates$surname<- trimws(toupper(gsub("[^[:alpha:] ]", "",names.delegates$surname))) 
delegates$first.name <- trimws(toupper(gsub("[^[:alpha:] ]", "",names.delegates$first.name))) 

delegates$sound.first <- soundex(delegates$first.name) # soundex of first name
delegates$sound.surname <- soundex(delegates$surname) # soundex of surname

# Make missing age NA
delegates$age[delegates$age==""] <- NA

# Make property values numeric
prop.vars <- c("realprop.60", "persprop.60", "realprop.70", "persprop.70")
for(var in prop.vars){
  delegates[,var] <- gsub("[^[:alnum:]]", "", delegates[,var])
  delegates[,var] <- as.numeric(delegates[,var])
}

# Make died binary
delegates$died[is.na(delegates$died)] <- 0

# Create taxable property variable
delegates$taxprop.60 <- delegates$realprop.60 + delegates$persprop.60
delegates$taxprop.70 <- delegates$realprop.70 + delegates$persprop.70

# Chain 1870 values to 1860 $
delegates$taxprop.70 <- delegates$taxprop.70/(8.06/12.65)
delegates$realprop.70 <- delegates$realprop.70/(8.06/12.65)
delegates$persprop.70 <- delegates$persprop.70/(8.06/12.65)

# Create measure of change in wealth between Censuses
delegates$taxprop.d <- delegates$taxprop.70-delegates$taxprop.60
delegates$realprop.d <- delegates$realprop.70 - delegates$realprop.60
delegates$persprop.d <- delegates$persprop.70 - delegates$persprop.60

# Estimate avg. values of slaves in 1860
delegates$slave.value <- delegates$slaves * 778

# Create former/future office dummies
delegates$former <- ifelse(delegates$former.office=="",0,1)
delegates$future <- ifelse(delegates$future.office=="",0,1)

# Create profession dummies
delegates$profession<- trimws(delegates$profession)
delegates$profession[delegates$profession=="Commission merchant" |
                       delegates$profession=="Commission"|
                       delegates$profession=="Clothing merchant"|
                       delegates$profession=="Flour merchant"|
                       delegates$profession=="Grocer- merchant"|
                       delegates$profession=="Grocery merchant"|
                       delegates$profession=="Retail merchant"|
                       delegates$profession=="Retired merchant"|
                       delegates$profession=="Dry goods merchant"|
                       delegates$profession=="Cotton broker"|
                       delegates$profession=="Cotton dealer"|
                       delegates$profession=="Dry goods merchant"|
                       delegates$profession=="Hide and wool dealer"] <- "Merchant" 

delegates$profession[delegates$profession=="Baptist minister" |
                       delegates$profession=="M.E. minister"|
                       delegates$profession=="Methodist minister"|
                       delegates$profession=="Presbyterian minister"] <- "Minister" 

delegates$profession[delegates$profession=="Planter" |
                       delegates$profession=="Rancher"] <- "Farmer" 

delegates$profession[delegates$profession=="Circuit ct. judge" |
                       delegates$profession=="Circuit ct. judge in 1868"|
                       delegates$profession=="Clerk"|
                       delegates$profession=="Court clerk"|
                       delegates$profession=="Judge of ordinary"|
                       delegates$profession=="Justice of peace"|
                       delegates$profession=="Justice of peace in 1850s"|
                       delegates$profession=="Law student"|
                       delegates$profession=="Police judge, 1870"|
                       delegates$profession=="U.S. marshal"|
                       delegates$profession=="Inspector of naval stores"|
                       delegates$profession=="Mayor of Augusta"|
                       delegates$profession=="Mayor of Corinth"|
                       delegates$profession=="Notary public"|
                       delegates$profession=="Postmaster of Key West"|
                       delegates$profession=="County treasurer, 1870"|
                       delegates$profession=="Customs house official"|
                       delegates$profession=="Customs house official house official"|
                       delegates$profession=="Customs collector"|
                       delegates$profession=="Registrar of bankruptcy"] <- "Law/Government Professional"


delegates$farmer <-ifelse(delegates$profession == "Farmer",1,0) 

delegates$lawyer <-ifelse(delegates$profession == "Lawyer"|
                            delegates$profession == "Law/Government Professional",1,0) # lawyer/law Professional

delegates$merchant <-ifelse(delegates$profession == "Merchant",1,0)

delegates$physician <-ifelse(delegates$profession == "Physician"|
                               delegates$profession == "Dentist"|
                               delegates$profession == "Druggist",1,0) #physician/dentist/druggist

delegates$minister <-ifelse(delegates$profession == "Minister",1,0) 

# Create bio dummies

unionist <- c(grep("Union",delegates$bio,T), grep("loyal",delegates$bio,T),
              grep("Anti-secessionist",delegates$bio,T),grep("Unionist",delegates$bio,T), 
              grep("Freedmen's Bureau",delegates$bio,T),
              grep("Peace advocate",delegates$bio,T), grep("carpetbagger",delegates$bio,T),
              grep("opponent of secession",delegates$bio,T),
              grep("ariti-secessionist",delegates$bio,T),
              grep("Pro-North",delegates$bio,T), grep("antipsecessionist",delegates$bio,T),
              grep("red string",delegates$bio,T), grep("Republican",delegates$bio,T),
              grep("Union",delegates$bio,T)) #unionist/union veteran/Republican/claimed loyalty during war

dem <- c(grep("Whig",delegates$bio,T), grep("Democrat",delegates$bio,T),
         grep("Democratic",delegates$bio,T),grep("pro-Douglas",delegates$bio,T),
         grep("Douglas",delegates$bio,T)) # former whig/Democrat

confederate <- c(grep("Confederate veteran",delegates$bio,T),
                 grep("Confederate captain",delegates$bio,T),grep("Confederate colonel",delegates$bio,T),
                 grep("Confederate officer",delegates$bio,T), grep("Confederate surgeon",delegates$bio,T),
                 grep("pro-secession",delegates$bio,T), grep("favored secession",delegates$bio,T),
                 grep("Confederate cavalry",delegates$bio,T), 
                 grep("Confederate lieutenant",delegates$bio,T), grep("Confederate It. colonel",delegates$bio,T),
                 grep("Confederate it. colonel",delegates$bio,T), grep("Confederate major",delegates$bio,T),
                 grep("Confederate veterao",delegates$bio,T), 
                 grep("served in Confederate",delegates$bio,T)) # confederate/secessionist

delegates$unionist <- 0
delegates$unionist[rownames(delegates) %in% unionist] <- 1

delegates$dem <- 0
delegates$dem[rownames(delegates) %in% dem] <- 1

delegates$confederate <- 0
delegates$confederate[rownames(delegates) %in% confederate] <- 1

## Merge delegates with votes
delegates$did <- 1:nrow(delegates) # create unique delegate identifier
votes$vid <- 1:nrow(votes) # create unique votes identifier

r.pairs <- compare.linkage(delegates[c("first.name", "surname", "state", "sound.first","sound.surname")],
                           votes[c("first.name", "surname", "state", "sound.first","sound.surname")])

#min.train <- getMinimalTrain(r.pairs,nEx=10) 
#min.train <- editMatch(min.train)
#saveRDS(min.train, paste0(data.directory,"min_train_delegates.rds"))
min.train <- readRDS(paste0(data.directory,"min_train_delegates.rds"))

model <- trainSupv(min.train, method = "svm")
result <- classifySupv(model, newdata = r.pairs)
summary(result)

links <- result$pairs[result$prediction=="L",][c("id1","id2")]
links <- links[!duplicated(links$id1),] # remove duplicates

delegates <- merge(delegates, links, by.x="did",by.y="id1", all.x=TRUE)
delegates <- merge(delegates, votes, by.x="id2",by.y="vid", all.x=TRUE,suffixes=c(".del",".votes"))
colnames(delegates)[1] <- "vid"
colnames(delegates)[3] <- "name"
colnames(delegates)[4] <- "state"
colnames(delegates)[26] <- "surname"
colnames(delegates)[27] <- "first.name"# for RLBigDataLinkage
colnames(delegates)[28] <- "sound.first"
colnames(delegates)[29] <- "sound.surname"

## Merge delegates/votes with pardons
pardons$pid <- 1:nrow(pardons) # create unique pardons identifier

# Merge by soundex surname and state
pardons.match <- merge(delegates,pardons,by=c("sound.surname","state")) 

#resort by delegate id
pardons.match <- pardons.match[order(pardons.match$did),]

# create match id
pardons.match$match.id <- 1:nrow(pardons.match)

# Create Jaro similarity measure on first name and surname
pardons.match$jaro.surname <- jarowinkler(pardons.match$surname.x,pardons.match$surname.y) 
pardons.match$jaro.first <- jarowinkler(pardons.match$first.name.x,pardons.match$first.name.y)

# Create exact match variables
pardons.match$exact.surname <- ifelse(pardons.match$surname.x==pardons.match$surname.y,1,0)
pardons.match$exact.first.name <- ifelse(pardons.match$first.name.x==pardons.match$first.name.y,1,0)

# Scale and center continuous vars
preProcValues <- preProcess(pardons.match[c("jaro.surname","jaro.first")], method = c("center", "scale")) 
pardons.match[c("jaro.surname","jaro.first")] <- predict(preProcValues, pardons.match[c("jaro.surname","jaro.first")])

# Split 1/3 
bound <- floor((nrow(pardons.match))*0.1)         #define % of training and test set

set.seed(42) # set seed for reproducibility
df <- pardons.match[sample(nrow(pardons.match)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.match <- 0
train.matches <- c(424,31,289,130,147,747) #match id
df.train$is.match[df.train$match.id %in% train.matches] <-1

# Create features vector
X.train <-df.train[c("jaro.surname","jaro.first","exact.surname","exact.first.name")]
X.test <-df.test[c("jaro.surname","jaro.first","exact.surname","exact.first.name")]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

jaro.first.mode <- Mode(X.train$jaro.first) # impute missing data with training mode
exact.first.mode <- Mode(X.train$exact.first.name)
X.train$jaro.first[is.na(X.train$jaro.first)] <- jaro.first.mode
X.train$exact.first.name[is.na(X.train$exact.first)] <- exact.first.mode

X.test$jaro.first[is.na(X.test$jaro.first)] <- jaro.first.mode
X.test$exact.first.name[is.na(X.test$exact.first)] <- exact.first.mode

# Create outcomes vector
Y.train <- as.matrix(df.train$is.match)

# Train 
# set.seed(42)
# fitSL.link <- SuperLearner(Y=Y.train[,1],
#                            X=data.frame(X.train),
#                            SL.library=SL.library.class,
#                            family="binomial") # glmnet response is 2-level factor

# Save prediciton model
saveRDS(fitSL.link, file = paste0(data.directory,"pardon_link.rds"))

# Print summary table
fitSL.link <- readRDS(paste0(data.directory,"pardon_link.rds"))

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$match.prob <- as.numeric(link.pred.test) # match probability 
X.test$match <- ifelse(X.test$match.prob>0.2,1,0) # 7 duplicates

# Merge training, test matches to delegates
train.matches <- df.train$did[df.train$is.match==1]
test.matches <- df.test$did[X.test$match==1]

delegates$pardon <- 0
delegates$pardon[delegates$did %in% c(train.matches, test.matches)] <- 1

## Preprocess matched data

# Subset data to nonmissing taxable property values
delegates.rd <- subset(delegates, !is.na(taxprop.60))

# Normalize continuous variables to 0-1
NormalizeIt <- function(x){
  x[!is.na(x)] <- (x[!is.na(x)]-min(x[!is.na(x)]))/(max(x[!is.na(x)])-min(x[!is.na(x)]))
  return(x)
}

vars.cont <- c("overall","per.black","age")
delegates.rd[,vars.cont]<- sapply(vars.cont, function(i){
  NormalizeIt(delegates.rd[,i])
})

# RD parameters
cutoff <- 20000 # define cutoff
upper <- 2*cutoff # define upper margin
