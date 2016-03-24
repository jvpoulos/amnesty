#####################################
### Power analysis                ###
#####################################

require(rdrobust)

# Define simulation parameters
alpha <- 0.05
L <- 100 # no. iterations 
r.prob <- c(0.03, 0.06, 0.09, 0.12) # probability of success in Bernouli trial
sd <- round(sd(delegates.rd$realprop.d, TRUE))
delta <- c(100, 1000, 10000,20000)
s.size <- c(500, 1000, 5000, 10000, 100000, 200000) # sample size

# Define RD parameters
rv <- ipums.60.1$taxprop # running variable to sample from 
cutoff <- 20000 # define cutoff
c.prob <- round(1-(13500/150000),1) # compliance rate for FRD

SimRD <- function(r.prob,delta, c.prob, s.size, rv, cutoff){
  # Simulate data under the null hypothesis
  design <- data.frame("rv" = sample(rv, s.size, replace=TRUE),
                       "tot"= 0)
  design$tot[design$rv >= cutoff] <- rbinom(nrow(design[design$rv >= cutoff,]), 1, c.prob)
  if(!is.null(r.prob)){
  design$response <- rbinom(s.size, 1, r.prob)
  }
  if(!is.null(delta)){
    design$response <- rnorm(s.size, delta, sd)
  } 
  # Fit the model
  fit <- rdrobust(design$response, 
                  design$rv,
                  c=cutoff,
                  fuzzy= design$tot,
                  all=TRUE,
                  bwselect="IK")
  # Return p value
  return(summary(fit)$coef[10])
}

p.vals.bin <- replicate(L,
  sapply(r.prob, function(r){ # columns:r.prob, rows: s.size
    sapply(s.size, function(s){
      SimRD(r,delta=NULL,c.prob, s, rv, cutoff)})
  }))

p.vals.bin.array <- t(sapply(1:L, function(i) array(p.vals.bin[,,i]))) # rows: iterations
saveRDS(p.vals.bin.array, paste0(data.directory,"power_p_values_bin.rds"))

p.vals.wealth <- replicate(L,
                        sapply(delta, function(d){ 
                          sapply(s.size, function(s){
                            SimRD(r.prob=NULL,d,c.prob, s, rv, cutoff)})
                        }))

p.vals.wealth.array <- t(sapply(1:L, function(i) array(p.vals.wealth[,,i]))) 
saveRDS(p.vals.wealth.array, paste0(data.directory,"power_p_values_wealth.rds"))

#power <- apply(p.vals.bin.array, 2, function (x) length(which(x < alpha))/L)
