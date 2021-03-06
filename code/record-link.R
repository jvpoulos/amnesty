## Link 1860 100% sample and slave sample to 1870 Census

train <- FALSE

# Merge by soundex surname, first initial, state and county

ipums.60 <- transform(ipums.60,merge.id=paste0(ipums.60$sound.surname, ipums.60$first.initial, " ", ipums.60$county, ", ", ipums.60$state))

slave.60 <- transform(slave.60,merge.id=paste0(slave.60$sound.surname, slave.60$first.initial, " ", slave.60$county, ", ", slave.60$state))

ipums.slave.link <- merge(ipums.60,slave.60,
                          by=c("merge.id")) 

# Create link id
ipums.slave.link <- ipums.slave.link[order(ipums.slave.link$pid),]
ipums.slave.link$slave.link.id <- 1:nrow(ipums.slave.link)

# Create Jaro similarity measure on first name and surname

ipums.slave.link$jaro.first <- jarowinkler(ipums.slave.link$first.x,ipums.slave.link$first.y)
ipums.slave.link$jaro.surname <- jarowinkler(ipums.slave.link$surname.x,ipums.slave.link$surname.y)

# Create exact link variables

ipums.slave.link$exact.first <- 0
ipums.slave.link$exact.surname <- 0
ipums.slave.link$exact.first[ipums.slave.link$first.x==ipums.slave.link$first.y] <- 1
ipums.slave.link$exact.surname[ipums.slave.link$surname.x==ipums.slave.link$surname.y] <- 1

ipums.slave.link$exact.first.length <- 0
ipums.slave.link$exact.surname.length <- 0
ipums.slave.link$exact.sound.first <- 0
ipums.slave.link$exact.first.length[ipums.slave.link$first.length.x==ipums.slave.link$first.length.y] <- 1
ipums.slave.link$exact.surname.length[ipums.slave.link$surname.length.x==ipums.slave.link$surname.length.y] <- 1
ipums.slave.link$exact.sound.first[ipums.slave.link$sound.first.x==ipums.slave.link$sound.first.y] <-1

# Scale and center continuous vars
preProcValues <- preProcess(ipums.slave.link[c("jaro.first","jaro.surname")], method = c("center", "scale")) 
ipums.slave.link[c("jaro.first","jaro.surname")] <- predict(preProcValues, ipums.slave.link[c("jaro.first","jaro.surname")])

# Split train/test 
bound.slave <- floor((nrow(ipums.slave.link))*0.1)         #define minimal training set

set.seed(42) # set seed for reproducibility
df.slave <- ipums.slave.link[sample(nrow(ipums.slave.link)), ]           #sample rows 
df.slave.train <- df.slave[1:bound.slave, ]              #get training set
df.slave.test <- df.slave[(bound.slave+1):nrow(df.slave), ]    #get test set

df.slave.train$is.link <- 0
slave.train.links <- c(df.slave.train$slave.link.id[ df.slave.train$exact.surname==1],
                       21106,10724,10797,16199,2026,10419,3820,7104,2988,4362,10848,5813,4148,
                       21016,480,3815,7031,14552,6504,12819,16455,16012,19072,10702,18685,20600,
                       10578,15730,2337,15845,15233,4490,8648,13329,11057,4195,1258,21025,20623,
                       3494,13403,16203,8764,15350,17172,587,9648,657,161,4204,11448,13482,
                       13284,17354,11694,5277,1694,15882,4029,4067,10569,13660,14998,17272,11009,
                       13856,17175,16024,8155,21199,10383,3503,21618,5633,16030,12145,21196,
                       18411,12322,11952,11627,13851,1685,10358,15031,391,8522,15165,4058,
                       21123,3102,1455,14157,5351,9641,8550,3069,101,8885,22586,10920,4825,
                       4998,6258,9663,7466,8173,4032,1468,1213,1676,17054,22112,5758,14358,
                       12329,8600,6509,15880,21588,16716,17585,15997) 

df.slave.train$is.link[df.slave.train$slave.link.id %in% slave.train.links] <-1

# Create features vector
slave.features <- c("jaro.first","jaro.surname","exact.first","exact.surname",
                    "exact.first.length","exact.surname.length","exact.sound.first")

X.slave.train <-df.slave.train[slave.features]
X.slave.test <-df.slave.test[slave.features]

jaro.first.mode <- Mode(X.slave.train$jaro.first) # impute missing data with training mode
X.slave.train$jaro.first[is.na(X.slave.train$jaro.first)] <- jaro.first.mode

X.slave.test$jaro.first[is.na(X.slave.test$jaro.first)] <- jaro.first.mode

# Create outcomes vector
Y.slave.train <- as.matrix(df.slave.train$is.link)

# Train 
if(train){
  set.seed(42)
  fitSL.slave.link <- SuperLearner(Y=Y.slave.train[,1],
                                   X=data.frame(X.slave.train),
                                   SL.library=SL.library.class,
                                   family="binomial") # glmnet response is 2-level factor
  
  #Save pred model
  saveRDS(fitSL.slave.link, file = paste0("data/ipums-slave-link.rds"))
}else{
  # Print summary table
  fitSL.slave.link <- readRDS("data/ipums-slave-link.rds")
  fitSL.slave.link
}

# Use response model to predict test
slave.link.pred.test <- predict(fitSL.slave.link, data.frame(X.slave.test))$pred

# Add predictions to test data
X.slave.test$is.link <- ifelse(as.numeric(slave.link.pred.test)>0.999,1,0) 

df.slave.test$is.link <- 0 
df.slave.test$is.link[df.slave.test$slave.link.id %in% df.slave.test$slave.link.id[X.slave.test$is.link==1]] <-1

# Merge training, test links to ipums
slave.link.df <- rbind(df.slave.train[df.slave.train$is.link==1,], df.slave.test[df.slave.test$is.link==1,])
slave.link.df <- slave.link.df[!duplicated(slave.link.df$pid),] # remove 1860 dups

# Select and rename slavepums vars
slave.link.df <- slave.link.df[c(2:47,56)] # remove link vars
names(slave.link.df) <- gsub(".x", "", names(slave.link.df), fixed = TRUE)

###############################

# Link 1860 slaveholders with 1870 Census by sound surname, first initial, and birthplace

slave.link.df <- transform(slave.link.df,merge.id=paste0(slave.link.df$sound.surname, slave.link.df$first.initial, " ", slave.link.df$birthplace))

ipums.70 <- transform(ipums.70,merge.id=paste0(ipums.70$sound.surname, ipums.70$first.initial, " ", ipums.70$birthplace))

ipums.link <- merge(slave.link.df,ipums.70,by=c("merge.id"))  

# Create link id
ipums.link <- ipums.link[order(ipums.link$pid.x),]
ipums.link$link.id <- 1:nrow(ipums.link)

# Make factor levels comparable
ipums.link$self_residence_place_county.x <- factor(ipums.link$self_residence_place_county.x, levels=levels(ipums.link$self_residence_place_county.y))

# Create Jaro similarity measure on first name and surname

ipums.link$jaro.first <- jarowinkler(ipums.link$first.x,ipums.link$first.y)
ipums.link$jaro.surname <- jarowinkler(ipums.link$surname.x,ipums.link$surname.y)

# Create exact link variables

ipums.link$exact.first <- 0
ipums.link$exact.middle.name <- 0
ipums.link$exact.surname <- 0
ipums.link$exact.first[ipums.link$first.x==ipums.link$first.y] <- 1
ipums.link$exact.middle.name[ipums.link$middle.name.x==ipums.link$middle.name.y] <- 1
ipums.link$exact.surname[ipums.link$surname.x==ipums.link$surname.y] <- 1

ipums.link$exact.first.length <- 0
ipums.link$exact.surname.length <- 0
ipums.link$exact.sound.first <- 0
ipums.link$exact.first.length[ipums.link$first.length.x==ipums.link$first.length.y] <- 1
ipums.link$exact.surname.length[ipums.link$surname.length.x==ipums.link$surname.length.y] <- 1
ipums.link$exact.sound.first[ipums.link$sound.first.x==ipums.link$sound.first.y] <-1

ipums.link$exact.state <- 0
ipums.link$exact.county <- 0
ipums.link$exact.state[ipums.link$state.x==ipums.link$state.y] <- 1
ipums.link$exact.county[ipums.link$self_residence_place_county.x==ipums.link$self_residence_place_county.y] <- 1

ipums.link$exact.age <- 0
ipums.link$exact.age[ipums.link$self_residence_info_age.x+10==ipums.link$self_residence_info_age.y] <- 1

# Scale and center continuous vars
preProcValues <- preProcess(ipums.link[c("jaro.first","jaro.surname")], method = c("center", "scale")) 
ipums.link[c("jaro.first","jaro.surname")] <- predict(preProcValues, ipums.link[c("jaro.first","jaro.surname")])

# Split train/test 
bound <- floor((nrow(ipums.link))*0.001)         #define minimal training set

set.seed(42) # set seed for reproducibility
df <- ipums.link[sample(nrow(ipums.link)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.link <- 0
train.links <- c(df.train$link.id[(df.train$exact.first + df.train$exact.surname)==2 |
                                    (df.train$exact.first + df.train$exact.surname + df.train$exact.state + df.train$exact.county + df.train$exact.age)>=3],
                 749405,792867,787958,254092,677719,393384,156054,749487,425576)

df.train$is.link[df.train$link.id %in% train.links] <-1

# Create features vector
features <- c("jaro.first","jaro.surname","exact.first","exact.middle.name","exact.surname",
              "exact.first.length","exact.surname.length","exact.sound.first","exact.state","exact.county","exact.age")

X.train <-df.train[features]
X.test <-df.test[features]

# Create outcomes vector
Y.train <- as.matrix(df.train$is.link)

# Train 
if(train){
  set.seed(42)
  fitSL.link <- SuperLearner(Y=Y.train[,1],
                             X=data.frame(X.train),
                             SL.library=SL.library.class,
                             family="binomial") # glmnet response is 2-level factor

  #Save pred model
  saveRDS(fitSL.link, file = "data/ipums-link.rds")
}else{
  # Print summary table
  fitSL.link <- readRDS("data/ipums-link.rds")
  fitSL.link
}

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$is.link <- ifelse(as.numeric(link.pred.test)>0.999,1,0) 

df.test$is.link <- 0 
df.test$is.link[df.test$link.id %in% df.test$link.id[X.test$is.link==1]] <-1

# Merge training, test links to ipums
link.df <- rbind(df.train[df.train$is.link==1,], df.test[df.test$is.link==1,])
link.df <- link.df[!duplicated(link.df$pid.x),] # remove 1860 dups

# Subset and rename 1870 vars
link.df <- subset(link.df, select=c("pid.x","StableURL.y","pid.y","self_empty_name_surname.y", "self_empty_name_given.y", "self_residence_place_state.y", 
                                    "self_residence_place_county.y","self_residence_place_city.y","self_residence_info_age.y"))

colnames(link.df) <- c("pid","StableURL.1870","pid.1870","self_empty_name_surname.1870", "self_empty_name_given.1870", "self_residence_place_state.1870", 
                       "self_residence_place_county.1870","self_residence_place_city.1870","self_residence_info_age.1870")

# Merged to 1860 linked sample
link.1860.1870 <- merge(slave.link.df, link.df, by=c("pid"),all.x=TRUE)

# Remove link variables
drops <- c("sound.surname","first.initial","state","county","surname","first","middle.name","surname.length","first.length","sound.first")
link.1860.1870 <- link.1860.1870[ , !(names(link.1860.1870) %in% drops)]

# Write linked sample to file
write.csv(link.1860.1870, "data/linked-sample-60-70.csv")