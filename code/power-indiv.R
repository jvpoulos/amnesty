#####################################
### Power analysis                ###
#####################################

library(ggplot2)
library(gridExtra)
library(rdrobust)

load("data/amnesty.RData") # load saved data

source("code/utils.R")

run.power <- TRUE

# Define simulation parameters
alpha <- 0.05
L <- 100 # no. iterations 
r.prob <- c(0.1,0.2, 0.3, 0.5, 0.7) # effect size for binary
delta <- c(1000,5000,10000,15000,20000) # effect size for continuous 
s.size.continuous <- c(400, 2500, 5000) # sample size
s.size.binary <- c(400,2500,5000) 

# Create grid for parameters
grid.wealth <- expand.grid("delta"=delta, "s.size"=s.size.continuous)
grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size.binary)

# Define RD parameters
rv <- slaveholders.60.rd$taxprop # running variable to sample from 

cutoff <- 20000 # define cutoff

if(run.power){
  p.vals.wealth <- replicate(L,
                             sapply(1:nrow(grid.wealth), function(i){
                               SimRD(r.prob=NULL,
                                     delta=grid.wealth$delta[i],
                                     s.size=grid.wealth$s.size[i],
                                     rv, cutoff)}))
  
  p.vals.wealth.array <- t(sapply(1:L, function(i) array(p.vals.wealth[,i])))  # rows: iterations
  saveRDS(p.vals.wealth.array, "data/power_p_values_wealth_indiv.rds")
} else{
  p.vals.wealth.array <- readRDS("data/power_p_values_wealth_indiv.rds")
}

grid.wealth$power <- apply(p.vals.wealth.array, 2, function (x) length(which(x < alpha))/L)

if(run.power){
  p.vals.bin <- replicate(L,
                          sapply(1:nrow(grid.bin), function(i){
                            SimRD(r.prob=grid.bin$r.prob[i],
                                  delta=NULL,
                                  s.size=grid.bin$s.size[i],
                                  rv, cutoff)}))
  
  p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))
  saveRDS(p.vals.bin.array, "data/power_p_values_bin.rds")
} else{
  p.vals.bin.array<- readRDS("data/power_p_values_bin_indiv.rds")
}

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/L)

#Create plots
power.plot.wealth <- ggplot(data=grid.wealth, aes(x=s.size, 
                                                  y=power, 
                                                  group = as.factor(delta), 
                                                  colour = as.factor(delta))) +
  geom_line() +
  scale_colour_discrete(name = "Effect size", labels=c("$1,000","$5,000","$10,000","$15,000","$20,000")) +
  scale_x_continuous(breaks=s.size.continuous, labels = c("400", "2,500", "5,000")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  ylab("") +
  xlab("") +
  ggtitle("Continuous response") + theme(plot.title = element_text(hjust = 0.5))

power.plot.bin <- ggplot(data=grid.bin, aes(x=s.size, 
                                            y=power, 
                                            group = as.factor(r.prob), 
                                            colour = as.factor(r.prob))) +
  geom_line() +
  scale_colour_discrete(name = "Effect size", labels=c("10%","20%","30%","50%", "70%")) +
  scale_x_continuous(breaks=s.size.binary, labels = c("400", "2,500", "5,000")) + 
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  ylab("") +
  xlab("") +
  ggtitle("Binary response") + theme(plot.title = element_text(hjust = 0.5))

# Combine plots

ggsave("data/plots/power-indiv.png", plot=grid.arrange(power.plot.wealth, power.plot.bin,
                                                             ncol=1, nrow=2, left="Power", bottom="Sample size") , scale=1.25)