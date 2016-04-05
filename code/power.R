#####################################
### Power analysis                ###
#####################################

require(rdrobust)

# Define simulation parameters
alpha <- 0.05
L <- 100 # no. iterations 
r.prob <- c(0.05, 0.25, 0.5) # effect size for binary
delta <- c(5000, 10000, 20000) # effect size for continuous 
s.size <- c(1000, 10000, 20000, 50000) # sample size

# Create grid for parameters
grid.wealth <- expand.grid("delta"=delta, "s.size"=s.size)
grid.bin <- expand.grid("r.prob"=r.prob, "s.size"=s.size)

# Define RD parameters
rv <- delegates.rd$taxprop.60 # running variable to sample from 
cutoff <- 20000 # define cutoff

SimRD <- function(r.prob,delta, s.size, rv, cutoff){
  # Simulate data
  design <-data.frame("rv"=sample(rv, s.size, replace=TRUE),
                      "response"=NA)
  if(!is.null(r.prob)){
    design$response[design$rv >= cutoff] <- rbinom(nrow(design[design$rv >= cutoff,]), 1, 0.1 + r.prob)
    design$response[design$rv < cutoff] <- rbinom(nrow(design[design$rv < cutoff,]), 1, 0.1)
  }
  if(!is.null(delta)){
    design$response[design$rv >= cutoff] <- rnorm(nrow(design[design$rv >= cutoff,]), 3000+delta, sd=25000)
    design$response[design$rv < cutoff] <- rnorm(nrow(design[design$rv < cutoff,]), 3000, sd=25000)
  } 
  # Fit the model
  fit <- rdrobust(design$response, 
                  design$rv,
                  c=cutoff,
                  bwselect="CCT")
  # Return p value
  return(summary(fit)$coef[11])
}

# p.vals.wealth <- replicate(L,
#                            sapply(1:nrow(grid.wealth), function(i){
#                              SimRD(r.prob=NULL,
#                                    delta=grid.wealth$delta[i],
#                                    s.size=grid.wealth$s.size[i],
#                                    rv, cutoff)}))
# 
# p.vals.wealth.array <- t(sapply(1:L, function(i) array(p.vals.wealth[,i])))  # rows: iterations
#saveRDS(p.vals.wealth.array, "power_p_values_wealth.rds")
p.vals.wealth.array<- readRDS(paste0(data.directory,"power_p_values_wealth.rds"))

grid.wealth$power <- apply(p.vals.wealth.array, 2, function (x) length(which(x < alpha))/L)

# p.vals.bin <- replicate(L,
#                         sapply(1:nrow(grid.bin), function(i){
#                           SimRD(r.prob=grid.bin$r.prob[i],
#                                 delta=NULL,
#                                 s.size=grid.bin$s.size[i],
#                                 rv, cutoff)}))
# 
# p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,i])))
#saveRDS(p.vals.bin.array, "power_p_values_bin.rds")
p.vals.bin.array<- readRDS(paste0(data.directory,"power_p_values_bin.rds"))

grid.bin$power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/L)

#Create plots
power.plot.wealth <- ggplot(data=grid.wealth, aes(x=s.size, 
                                                  y=power, 
                                                  group = as.factor(delta), 
                                                  colour = as.factor(delta))) +
  geom_line() +
  scale_colour_discrete(name = "Effect size", labels=c("$5,000","$10,000", "$20,000")) +
  scale_x_continuous(breaks=s.size, labels = c("1,000", "10,000", "20,000", "50,000")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  geom_vline(xintercept = 35000, colour="red", linetype = 2) +
  ylab("") +
  xlab("") +
  ggtitle("Continuous response (e.g., change in census wealth, 1860-1870)")

power.plot.bin <- ggplot(data=grid.bin, aes(x=s.size, 
                                                  y=power, 
                                                  group = as.factor(r.prob), 
                                                  colour = as.factor(r.prob))) +
  geom_line() +
  scale_colour_discrete(name = "Probability", labels=c("5%","25%", "50%")) +
  scale_x_continuous(breaks=s.size, labels = c("1,000", "10,000", "20,000", "50,000")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  geom_vline(xintercept = 35000, colour="red", linetype = 2) +
  ylab("") +
  xlab("") +
  ggtitle("Binary response (e.g., ex-post officeholding)")

# Combine plots
pdf(paste0(data.directory,"plots/power-plots.pdf"), width=8.27, height=11.69)
print(grid.arrange(power.plot.wealth, power.plot.bin,
                   ncol=1, nrow=2, left="Power", bottom="Sample size")) 
dev.off() 