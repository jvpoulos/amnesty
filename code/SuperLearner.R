# Packages
require(SuperLearner)
require(class)
require(randomForest)
require(glmnet)
require(gam)
require(e1071)
require(gbm)
require(nnet)

# Create randomForest wrappers changing both mtry and nodesize
tuneGrid <- expand.grid(mtry=c(1,5,10), nodesize=c(1,5))
create.SL.randomForest <- function(tune = list(mtry = c(1, 5, 10), nodesize = c(1, 5))) {
  tuneGrid <- expand.grid(tune, stringsAsFactors = FALSE)
  for(mm in seq(nrow(tuneGrid))) { 
    eval(parse(file = "", text = paste("SL.randomForest.", mm, "<- function(..., mtry = ", tuneGrid[mm, 1], ", nodesize = ", tuneGrid[mm, 2], ") SL.randomForest(..., mtry = mtry, nodesize = nodesize)", sep = "")), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.randomForest()

# Create glmnet wrappers in the global environment with different alpha. The default value for alpha in SL.glmnet is 1
create.SL.glmnet <- function(alpha = c(0,0.25, 0.50, 0.75)) {
  for(mm in seq(length(alpha))){
    eval(parse(text = paste('SL.glmnet.', alpha[mm], '<- function(..., alpha = ', alpha[mm], ') SL.glmnet(..., alpha = alpha)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create.SL.glmnet()


# Create gam wrappers in the global environment with different degrees. The default value for deg.gam in SL.gam is 2
create.SL.gam <- function(deg.gam = c(3, 4)) {
  for(mm in seq(length(deg.gam))){
    eval(parse(text = paste('SL.gam.', deg.gam[mm], '<- function(..., deg.gam = ', deg.gam[mm], ') SL.gam(..., deg.gam = deg.gam)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}

create.SL.gam()

# Define library
SL.library.class<- c("SL.gbm",
                     "SL.glmnet", # lasso
                     "SL.glmnet.0", # ridge
                     "SL.glmnet.0.25",
                     "SL.glmnet.0.5",
                     "SL.glmnet.0.75",
                     "SL.nnet",
                     "SL.randomForest",
                     "SL.randomForest.1", # nodesize=1 for regression
                     "SL.randomForest.2",
                     "SL.randomForest.3")

SL.library.reg <- c("SL.gam", # degree=2
                    "SL.gam.3",
                    "SL.gam.4",
                    "SL.gbm",
                    "SL.glm",
                    "SL.glmnet", # lasso
                    "SL.glmnet.0", # ridge
                    "SL.glmnet.0.25",
                    "SL.glmnet.0.5",
                    "SL.glmnet.0.75",
                    "SL.randomForest",
                    "SL.randomForest.4", # nodesize=5 for regression
                    "SL.randomForest.5",
                    "SL.randomForest.6",
                    "SL.svm")
