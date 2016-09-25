run.descriptive <- FALSE
run.power <-FALSE

# Libraries
require(reshape2)
require(RecordLinkage)
require(ggplot2)
require(rdrobust)
require(doParallel)
require(data.table)
require(caret)

if(run.descriptive){
  require(reporttools)
  require(weights)
  require(plyr)
  require(gridExtra)
  require(reshape)
  require(scales)
}

# Set directories
data.directory <- "~/Dropbox/github/amnesty/data/"
code.directory <- "~/Dropbox/github/amnesty/code/"

setwd(code.directory)

# Source scripts (in order)
source("utils.R")
source("SuperLearner.R")
source("pardons")
source("votes.R")
source("delegates.R") 
source("rd-plots.R") 
source("rd-balance.R") # req. rddensity.R, rddensity_fun.R, and rdbwdensity.R
source("rd-estimates.R") 

if(run.descriptive){
  source("ipums-1850.R")
  source("ipums-1860.R")
  source("descriptive.R")
}

if(run.power){
  source("power.R")
}