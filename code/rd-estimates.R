# RD estimates for summary figures

require(ggplot2)
require(rdrobust)

#source("delegates.R") # Run delegates
#source("rd-balance.R") # Create balance plots 

# Create vector for responsement variables
response.vars <- c("realprop.d","persprop.d","taxprop.d","future","overall","race","misc","gov","suffrage","econ","protest")

# Normalize response variables  
# delegates.rd <- sapply(response.vars, function(i) scale(delegates.rd[,i]))

# Apply rdrobust over characteristics
cct.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                           delegates.rd$taxprop.60,
                                                           c=cutoff,
                                                           all=TRUE,
                                                           bwselect="CCT")) 

ik.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="IK")) 

cv.response <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV")) 
# Create data for plot
balance.dat.r <- data.frame(x = c("Change in real estate value, 1860-1870 (1860$)",
                                  "Change in personal property value, 1860-1870 (1860$)",
                                  "Change in taxable property value, 1860-1870 (1860$)",
                                  "Future officeholder",
                                  "RSS: overall",
                                  "RSS: race",
                                  "RSS: misc.",
                                  "RSS: gov. structure",
                                  "RSS: suffrage",
                                  "RSS: economics",
                                  "Protested adoption of state constitution"),
                          y = c(sapply(cct.response, "[[", "coef")[1,],
                                sapply(ik.response, "[[", "coef")[1,],
                                sapply(cv.response, "[[", "coef")[1,]),
                          y.lo = c(sapply(cct.response, "[[", "ci")[3,],
                                   sapply(ik.response, "[[", "ci")[3,],
                                   sapply(cv.response, "[[", "ci")[3,]),
                          y.hi = c(sapply(cct.response, "[[", "ci")[6,],
                                   sapply(ik.response, "[[", "ci")[6,],
                                   sapply(cv.response, "[[", "ci")[6,]),
                          N = c(rep(sapply(cct.response, "[[", "N"),3))) 

# balance.dat.r$Estimate<- c(rep("Conv. (conv. CI)",11),
#                          rep("Bias-corrected (conv. CI)",11),
#                          rep("Bias-corrected (robust CI)",11))

balance.dat.r$Bandwidth <- c(rep("CCT bandwidth",11),
                           rep("IK bandwidth",11),
                           rep("CV bandwidth",11)) # all use conventional estimates with robust CIs

# Plot forest plot
suppressWarnings(balance.dat.r$x <- factor(balance.dat.r$x, levels=rev(balance.dat.r$x))) # reverse order

pdf(paste0(data.directory,"plots/rd_estimates.pdf"), width=11.69, height=8.27)
ForestPlot(balance.dat.r[balance.dat.r$x != "Change in real estate value, 1860-1870 (1860$)" & 
                           balance.dat.r$x != "Change in personal property value, 1860-1870 (1860$)" &
                           balance.dat.r$x != "Change in taxable property value, 1860-1870 (1860$)",],
           xlab="Regression discontinuity estimate",ylab="") +
 # scale_y_continuous(breaks = c(-1,0,1), labels = c("-1", "0", "1")) + 
  facet_grid(.~Bandwidth)
dev.off() 

pdf(paste0(data.directory,"plots/rd_estimates_cont.pdf"), width=11.69, height=8.27)
ForestPlot(balance.dat.r[balance.dat.r$x== "Percent black" | balance.dat.r$x== "Age",],
           xlab="Regression discontinuity estimate",ylab="") +
  facet_grid(.~Bandwidth)
dev.off() 