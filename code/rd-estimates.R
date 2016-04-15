########################
### RD estimates     ###
########################

# Import rdlocalrand functions
source(paste0(code.directory,'rdlocrand/rdlocrand_fun.R'))
source(paste0(code.directory,"rdlocrand/rdwinselect.R"))
source(paste0(code.directory,"rdlocrand/rdrandinf.R"))
source(paste0(code.directory,"rdlocrand/rdsensitivity.R"))
source(paste0(code.directory,"rdlocrand/rdrbounds.R"))

# Select predetermined covariates to be used for window selector 
X <- array(delegates.rd[pretreat.vars])

# Assign names to the covariates
colnames(X) <-  covars.names

# Summary figure for estimates
ForestPlot <- function(d, xlab, ylab){
  # Forest plot for summary figure
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=x)) + 
    geom_pointrange(size=1, alpha=0.8) + 
    coord_flip() +
    geom_hline(aes(x=0), lty=2) +
    theme(legend.position="none") +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

# Create vector for responsement variables
response.vars <- c("persprop.d","realprop.d","taxprop.d","future","protest","overall","econ","gov","misc","race","suffrage")

# Apply rdrobust over responses
cct.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                           delegates.rd$taxprop.60,
                                                           c=cutoff,
                                                           all=TRUE,
                                                           bwselect="CCT",
                                                           kernel="uniform")) 

ik.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="IK",
                                                          kernel="uniform")) 

cv.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV",
                                                          kernel="uniform")) 

# # Randomization inference using recommended window using rdwinselect
# rand.wealth <- lapply(response.vars[1:3], function(i) rdrandinf(Y=delegates.rd[,i],
#                                                            R=delegates.rd$taxprop.60,
#                                                            cutoff=cutoff,
#                                                            statistic="ttest",
#                                                            rdwstat = "ttest",
#                                                            covariates=X,
#                                                            kernel = "uniform",
#                                                            reps=10000,
#                                                            wmin=500,
#                                                            wstep=100,
#                                                            nwindows = 20,
#                                                            rdwreps= 10000,
#                                                            level = .1,
#                                                            quietly = TRUE,
#                                                            ci = c(0.05, seq(-12000, 25000, by=1)))) 

#saveRDS(rand.wealth, "rand_wealth.rds")
rand.wealth <- readRDS(paste0(data.directory,"rand_wealth.rds"))

# rand.bin <- lapply(response.vars[4:length(response.vars)], function(i) rdrandinf(Y=delegates.rd[,i],
#                                                                 R=delegates.rd$taxprop.60,
#                                                                 cutoff=cutoff,
#                                                                 statistic="ttest",
#                                                                 rdwstat = "ttest",
#                                                                 covariates=X,
#                                                                 kernel = "uniform",
#                                                                 reps=10000,
#                                                                 wmin=500,
#                                                                 wstep=100,
#                                                                 nwindows = 20,
#                                                                 rdwreps=10000,
#                                                                 level = .1,
#                                                                 quietly = TRUE,
#                                                                 ci = c(0.05, seq(-1.5, 1.5, by=.0001))))

# saveRDS(rand.bin, "rand_bin.rds")
rand.bin <- readRDS(paste0(data.directory,"rand_bin.rds"))

# Create data for plot
response.dat <- data.frame(x = c("Personal property value",
                                 "Real estate value",
                                 "Total census wealth",
                                 "Ex-post officeholder",
                                 "Protested adoption of constitution",
                                 "RSS: overall",
                                 "RSS: economics",
                                 "RSS: gov. structure",
                                 "RSS: misc.",
                                 "RSS: race",
                                 "RSS: suffrage"),
                           y = c(sapply(cct.response, "[[", "coef")[3,], # CCT: robust estimates
                                 sapply(ik.response, "[[", "coef")[1,],  # IK: conventional estimates
                                 sapply(cv.response, "[[", "coef")[1,], # CV: conventional estimates
                                 sapply(rand.wealth, "[[", "obs.stat"), # Difference-in means
                                 sapply(rand.bin, "[[", "obs.stat")), 
                           y.lo = c(sapply(cct.response, "[[", "ci")[3,], # CCT: robust CIs
                                    sapply(ik.response, "[[", "ci")[1,], # IK: conventional CIs
                                    sapply(cv.response, "[[", "ci")[1,], # CV: conventional CIs
                                    sapply(rand.wealth, "[[", "ci")[1,], # Randomization CI
                                    sapply(rand.bin, "[[", "ci")[1,]),
                           y.hi = c(sapply(cct.response, "[[", "ci")[5,],
                                    sapply(ik.response, "[[", "ci")[4,],
                                    sapply(cv.response, "[[", "ci")[4,],
                                    sapply(rand.wealth, "[[", "ci")[2,], # Randomization CI
                                    sapply(rand.bin, "[[", "ci")[2,]),
                           N = c(rep(sapply(cct.response, "[[", "N"),4))) # no. of methods

response.dat$Method <- c(rep("CCT",length(response.vars)),
                            rep("IK",length(response.vars)),
                            rep("CV",length(response.vars)),
                            rep("RI",length(response.vars)))

# Plot forest plot
suppressWarnings(response.dat$x <- factor(response.dat$x, levels=rev(response.dat$x))) # reverse order

pdf(paste0(data.directory,"plots/rd_estimates_bin.pdf"), width=11.69, height=8.27)
ForestPlot(response.dat[response.dat$x != "Real estate value" & 
                          response.dat$x != "Personal property value" &
                          response.dat$x != "Total census wealth",],
           xlab="Treatment effect estimate",ylab="") +
   scale_y_continuous(breaks = c(-1,0,1), labels = c("-1", "0", "1")) + 
  facet_grid(.~Method)
dev.off() 

pdf(paste0(data.directory,"plots/rd_estimates_wealth.pdf"), width=11.69, height=8.27)
ForestPlot(response.dat[response.dat$x == "Real estate value" | 
                          response.dat$x == "Personal property value" |
                          response.dat$x == "Total census wealth",],
           xlab="Treatment effect estimate",ylab="Change in census wealth, 1860-1870 (1860$)") +
  scale_y_continuous(breaks = c(-10000,0,20000), labels = c("-10,000", "0", "20,000")) + 
  facet_grid(.~Method)
dev.off() 
