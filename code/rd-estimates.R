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
#colnames(X) <-  covars.names

# Summary figure for estimates
ForestPlot <- function(d, xlab, ylab){
  # Forest plot for summary figure
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=Model)) + 
    geom_pointrange(size=1, alpha=0.5) + 
    coord_flip() +
    geom_hline(aes(x=0), lty=2) +
  #  theme(legend.position="top") +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}

# Create vector for responsement variables
response.vars <- c("persprop.70","realprop.70","taxprop.70","future","protest","overall","econ","gov","misc","race","suffrage")

# Apply rdrobust over responses

cct.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                               delegates.rd$taxprop.60,
                                                               c=cutoff,
                                                               all=TRUE,
                                                               bwselect="CCT",
                                                               kernel="uniform")) 

cct.response.frd <- lapply(1:length(response.vars), function(i){
  iv.result <- ivreg(delegates.rd[,response.vars[i]] ~ tot | treat, subset= ((realprop.60 >= cutoff & realprop.60 <= (cutoff+cct.response.srd[[i]]$h)) |
                                                                               (realprop.60 < cutoff & realprop.60 >= (cutoff-cct.response.srd[[i]]$h))),
                     data=delegates.rd)
  return(list("CI"= c(confint(iv.result)[2],confint(iv.result)[4]),
              "TOT.ATE"=iv.result$coefficients[[2]]))})

ik.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="IK",
                                                          kernel="uniform")) 

ik.response.frd <- lapply(1:length(response.vars), function(i){
  iv.result <- ivreg(delegates.rd[,response.vars[i]] ~ tot | treat, subset= ((realprop.60 >= cutoff & realprop.60 <= (cutoff+ik.response.srd[[i]]$h)) |
                                                                               (realprop.60 < cutoff & realprop.60 >= (cutoff-ik.response.srd[[i]]$h))),
                     data=delegates.rd)
  return(list("CI"= c(confint(iv.result)[2],confint(iv.result)[4]),
              "TOT.ATE"=iv.result$coefficients[[2]]))})

cv.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV",
                                                          kernel="uniform")) 

cv.response.frd <- lapply(1:length(response.vars), function(i){
  iv.result <- ivreg(delegates.rd[,response.vars[i]] ~ tot | treat, subset= ((realprop.60 >= cutoff & realprop.60 <= (cutoff+cv.response.srd[[i]]$h)) |
                                                                               (realprop.60 < cutoff & realprop.60 >= (cutoff-cv.response.srd[[i]]$h))),
                     data=delegates.rd)
  return(list("CI"= c(confint(iv.result)[2],confint(iv.result)[4]),
              "TOT.ATE"=iv.result$coefficients[[2]]))})

# # Randomization inference using recommended window using rdwinselect
# rand.wealth.srd <- lapply(response.vars[1:3], function(i) rdrandinf(Y=delegates.rd[,i],
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

# rand.wealth.frd <- lapply(response.vars[1:3], function(i) rdrandinf(Y=delegates.rd[,i],
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
#rand.wealth <- readRDS(paste0(data.directory,"rand_wealth.rds"))

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
#rand.bin <- readRDS(paste0(data.directory,"rand_bin.rds"))

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
                           y = c(sapply(cct.response.srd, "[[", "coef")[3,], # CCT: robust estimates
                                 sapply(cct.response.frd, "[[", "TOT.ATE"),
                                 sapply(ik.response.srd, "[[", "coef")[1,],  # IK: conventional estimates
                                 sapply(ik.response.frd, "[[", "TOT.ATE"),
                                 sapply(cv.response.srd, "[[", "coef")[1,], # CV: conventional estimates
                                 sapply(cv.response.frd, "[[", "TOT.ATE")),
#                                  sapply(rand.wealth, "[[", "obs.stat"), # Difference-in means
#                                  sapply(rand.bin, "[[", "obs.stat")), 
                           y.lo = c(sapply(cct.response.srd, "[[", "ci")[3,], # CCT: robust CIs
                                    sapply(cct.response.frd, "[[", "CI")[1,],
                                    sapply(ik.response.srd, "[[", "ci")[1,], # IK: conventional CIs
                                    sapply(ik.response.frd, "[[", "CI")[1,],
                                    sapply(cv.response.srd, "[[", "ci")[1,], # CV: conventional CIs
                                    sapply(cv.response.frd, "[[", "CI")[1,]),
#                                     sapply(rand.wealth, "[[", "ci")[1,], # Randomization CI
#                                     sapply(rand.bin, "[[", "ci")[1,]),
                           y.hi = c(sapply(cct.response.srd, "[[", "ci")[5,],
                                    sapply(cct.response.frd, "[[", "CI")[2,],
                                    sapply(ik.response.srd, "[[", "ci")[4,],
                                    sapply(ik.response.frd, "[[", "CI")[2,],
                                    sapply(cv.response.srd, "[[", "ci")[4,],
                                    sapply(cv.response.frd, "[[", "CI")[2,])) 
#                                     sapply(rand.wealth, "[[", "ci")[2,], # Randomization CI
#                                     sapply(rand.bin, "[[", "ci")[2,]))

response.dat$Method <- c(rep("CCT",length(response.vars), each=2),
                         rep("IK",length(response.vars), each=2),
                         rep("CV",length(response.vars), each=2))

response.dat$Model <- rep(rep(c("SRD","FRD (2SLS)"),each=length(response.vars)),3)

response.dat$Interact <- interaction(response.dat$Method, response.dat$Model)

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
           xlab="Treatment effect estimate",ylab="1870 census wealth (1860$)") +
  scale_y_continuous(breaks = c(-80000,-40000,0,40000), labels = c("-80,000", "-40,000", "0", "40,000")) + 
  facet_grid(.~Method)
dev.off() 
