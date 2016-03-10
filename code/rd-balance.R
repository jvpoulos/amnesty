# RD balance plots

require(ggplot2)
require(rdrobust)

#source("delegates.R") # Run delegates first

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

# Create vector for pretreatment variables
pretreat.vars <- c("per.black","age", 
                   "farmer","lawyer","merchant","physician", "minister",
                   "former","unionist","dem","confederate")

# Apply rdrobust over characteristics
cct.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                           delegates.rd$taxprop.60,
                                                           c=cutoff,
                                                           all=TRUE,
                                                           bwselect="CCT")) 

ik.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                           delegates.rd$taxprop.60,
                                                           c=cutoff,
                                                           all=TRUE,
                                                           bwselect="IK")) 

cv.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV")) 
# Create data for plot
balance.dat <- data.frame(x = c("Percent black", "Age", 
                                "Farmer", "Lawyer","Merchant","Physician", "Minister",
                                "Former officeholder", "Unionist", "Democrat", "Confederate"),
                        y = c(sapply(cct.pretreat, "[[", "coef")[1,],
                              sapply(ik.pretreat, "[[", "coef")[1,],
                              sapply(cv.pretreat, "[[", "coef")[1,]),
                        y.lo = c(sapply(cct.pretreat, "[[", "ci")[3,],
                              sapply(ik.pretreat, "[[", "ci")[3,],
                              sapply(cv.pretreat, "[[", "ci")[3,]),
                        y.hi = c(sapply(cct.pretreat, "[[", "ci")[6,],
                                 sapply(ik.pretreat, "[[", "ci")[6,],
                                 sapply(cv.pretreat, "[[", "ci")[6,]),
                        N = c(rep(sapply(cct.pretreat, "[[", "N"),3))) 

# balance.dat$Estimate<- c(rep("Conv. (conv. CI)",11),
#                          rep("Bias-corrected (conv. CI)",11),
#                          rep("Bias-corrected (robust CI)",11)

balance.dat$Bandwidth <- c(rep("CCT bandwidth",11),
                            rep("IK bandwidth",11),
                            rep("CV bandwidth",11)) # all use conventional estimates with robust CIs

# Plot forest plot
suppressWarnings(balance.dat$x <- factor(balance.dat$x, levels=rev(balance.dat$x))) # reverse order

pdf(paste0(data.directory,"plots/rd_balance.pdf"), width=11.69, height=8.27)
ForestPlot(balance.dat[balance.dat$x != "Percent black" & balance.dat$x != "Age",],
           xlab="Regression discontinuity estimate",ylab="") +
  scale_y_continuous(breaks = c(-1,0,1), labels = c("-1", "0", "1")) + 
  facet_grid(.~Bandwidth)
dev.off() 

pdf(paste0(data.directory,"plots/rd_balance_cont.pdf"), width=11.69, height=8.27)
ForestPlot(balance.dat[balance.dat$x== "Percent black" | balance.dat$x== "Age",],
           xlab="Regression discontinuity estimate",ylab="") +
  facet_grid(.~Bandwidth)
dev.off() 