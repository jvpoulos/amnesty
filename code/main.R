run.eda <- TRUE

# Libraries
require(reshape2)
require(RecordLinkage)
require(ggplot2)
require(rdrobust)

if(run.eda){
#  require(reporttools)
#  require(weights)
  require(plyr)
}

# Set directories
data.directory <- "~/Dropbox/github/amnesty/data/"
code.directory <- "~/Dropbox/github/amnesty/code/"
options(fftempdir = data.directory) 

setwd(code.directory)

# Source scripts
source("votes.R")
source("delegates.R") 
source("rd-plots.R") 
source("rd-balance.R") # req. rddensity.R, rddensity_fun.R, and rdbwdensity.R
source("rd-estimates.R")

if(run.eda){
  source("StFirst.R") # standardize first names fn
  source("ipums-1850.R")
  source("ipums-1860.R")
  source("eda.R")
}