# Libraries
library(reshape2)
library(RecordLinkage)
library(ggplot2)
options(bitmapType = 'cairo', device = 'png')
library(rdrobust)
library(doParallel)
library(data.table)
library(caret)
library(phonics)
library(readxl)
library(dplyr)

library(reporttools)
library(weights)
library(plyr)
library(gridExtra)
library(reshape)
library(scales)

source("code/utils.R")
source("code/SuperLearner.R")

# Prepare data

source("code/pardons.R") # pardon grants data
source("code/votes.R") # delegate votes
source("code/delegates.R") # Merge southern white delegates with votes and pardons (loads trained model)

source("code/slaveholders-1860.R") # 1860 slaveholders (loads trained model)

# RD plots and estimates
source("code/rd-plots.R") # RD plots
source("code/rd-balance.R") # RD balance plot for delegates
source("code/rd-balance-slaveholders.R") # RD balance plot for 1860 slaveholders
source("code/rd-estimates.R") # RD estimates for delegates
source("code/rd-estimates-slaveholders.R") # RD estimates for 1860 slaveholders

## Descriptive plots
source("code/ipums-1860.R")
source("code/ipums-1870.R")
source("code/record-link.R") # defaults to no train
source("code/descriptive.R")

save.image("data/amnesty.RData")