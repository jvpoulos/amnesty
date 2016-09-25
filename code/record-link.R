## Link 1860 100% sample and slave sample, 1870 Census

# # Subset samples to GA
# ipums.60.ga <- ipums.60[ipums.60$self_residence_place_state=="Georgia",]
# 
# slave.60.ga <- slave.60[slave.60$state=="Georgia",]

# Merge by soundex surname and state/county
ipums.60$pid <- 1:nrow(ipums.60) # create unique ipums identifier

ipums.link <- merge(ipums.60,slave.60,by=c("sound.surname","state","county")) 

# create link id
ipums.link <- ipums.link[order(ipums.link$pid),]
ipums.link$link.id <- 1:nrow(ipums.link)

# Create Jaro similarity measure on first name and surname
ipums.link$jaro.surname <- jarowinkler(ipums.link$surname.x,ipums.link$surname.y) 
ipums.link$jaro.first <- jarowinkler(ipums.link$first.x,ipums.link$first.y)

# Create exact link variables
ipums.link$exact.surname <- ifelse(ipums.link$surname.x==ipums.link$surname.y,1,0)
ipums.link$exact.first <- ifelse(ipums.link$first.x==ipums.link$first.y,1,0)

# Scale and center continuous vars
preProcValues <- preProcess(ipums.link[c("jaro.surname","jaro.first")], method = c("center", "scale")) 
ipums.link[c("jaro.surname","jaro.first")] <- predict(preProcValues, ipums.link[c("jaro.surname","jaro.first")])

# Split train/test 
bound <- floor((nrow(ipums.link))*0.01)         #define % of training and test set

set.seed(42) # set seed for reproducibility
df <- ipums.link[sample(nrow(ipums.link)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]    #get test set

df.train$is.link <- 0
train.links <- c(85528,4872,22287,98838,1026,205,30406,27135,28173,95383,79210,75409,103281,11252,
                 26641,41478,42645,61086,6309,115863,89514,67977,25824,3466,128711,92645,26761,61120,
                 19498,67408,106723,43903,69289,81811,121083,105184,1492,66418,122035,76029,120541,
                 5843,55943,84863,88654,128321,58024,62917,28165,11314) #link id
df.train$is.link[df.train$link.id %in% train.links] <-1

# Create features vector
X.train <-df.train[c("jaro.surname","jaro.first","exact.surname","exact.first")]
X.test <-df.test[c("jaro.surname","jaro.first","exact.surname","exact.first")]

jaro.first.mode <- Mode(X.train$jaro.first) # impute missing data with training mode
exact.first.mode <- Mode(X.train$exact.first)
X.train$jaro.first[is.na(X.train$jaro.first)] <- jaro.first.mode
X.train$exact.first[is.na(X.train$exact.first)] <- exact.first.mode

X.test$jaro.first[is.na(X.test$jaro.first)] <- jaro.first.mode
X.test$exact.first[is.na(X.test$exact.first)] <- exact.first.mode

# Create outcomes vector
Y.train <- as.matrix(df.train$is.link)

# Train 
# set.seed(42)
# fitSL.link <- SuperLearner(Y=Y.train[,1],
#                            X=data.frame(X.train),
#                            SL.library=SL.library.class,
#                            family="binomial") # glmnet response is 2-level factor
# 
# #Save prediciton model
# saveRDS(fitSL.link, file = paste0(data.directory,"ipums-60-link.rds"))

# Print summary table
fitSL.link <- readRDS(paste0(data.directory,"ipums-60-link.rds"))
fitSL.link

# Use response model to predict test
link.pred.test <- predict(fitSL.link, data.frame(X.test))$pred

# Add predictions to test data
X.test$link.prob <- as.numeric(link.pred.test) # link probability 
X.test$link <- ifelse(X.test$link.prob>0.007,1,0) 

# Merge training, test links to ipums
train.links.pid <- df.train$pid[df.train$is.link==1]
test.links.pid <- unique(df.test$pid[X.test$link==1]) # duplicates

ipums.60$slaveholder <- 0
ipums.60$slaveholder[ipums.60$pid %in% c(train.links.pid, test.links.pid)] <- 1

  