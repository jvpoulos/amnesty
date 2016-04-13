########################
### RD estimates     ###
########################

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

# Apply rdrobust over characteristics
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
# Create data for plot
response.dat <- data.frame(x = c("Change in personal property value, 1860-1870 (1860$)",
                                  "Change in real estate value, 1860-1870 (1860$)",
                                  "Change in total census wealth, 1860-1870 (1860$)",
                                  "Ex-post officeholder",
                                  "Protested adoption of constitution",
                                  "RSS: overall",
                                  "RSS: economics",
                                  "RSS: gov. structure",
                                  "RSS: misc.",
                                  "RSS: race",
                                  "RSS: suffrage"),
                          y = c(sapply(cct.response, "[[", "coef")[2,], # CCT: bias-corrected estimates
                                sapply(ik.response, "[[", "coef")[1,],  # IK: conventional estimates
                                sapply(cv.response, "[[", "coef")[1,]), # CV: conventional estimates
                          y.lo = c(sapply(cct.response, "[[", "ci")[2,], # CCT: bias-corrected CIs
                                   sapply(ik.response, "[[", "ci")[1,], # IK: conventional CIs
                                   sapply(cv.response, "[[", "ci")[1,]), # CV: conventional CIs
                          y.hi = c(sapply(cct.response, "[[", "ci")[5,],
                                   sapply(ik.response, "[[", "ci")[4,],
                                   sapply(cv.response, "[[", "ci")[4,]),
                          N = c(rep(sapply(cct.response, "[[", "N"),3))) 

response.dat$Bandwidth <- c(rep("CCT",11),
                           rep("IK",11),
                           rep("CV",11)) # all use conventional estimates with robust CIs

# Plot forest plot
suppressWarnings(response.dat$x <- factor(response.dat$x, levels=rev(response.dat$x))) # reverse order

pdf(paste0(data.directory,"plots/rd_estimates_bin.pdf"), width=11.69, height=8.27)
ForestPlot(response.dat[response.dat$x != "Change in real estate value, 1860-1870 (1860$)" & 
                           response.dat$x != "Change in personal property value, 1860-1870 (1860$)" &
                           response.dat$x != "Change in total census wealth, 1860-1870 (1860$)",],
           xlab="RD estimate",ylab="") +
 # scale_y_continuous(breaks = c(-1,0,1), labels = c("-1", "0", "1")) + 
  facet_grid(.~Bandwidth)
dev.off() 

pdf(paste0(data.directory,"plots/rd_estimates_wealth.pdf"), width=11.69, height=8.27)
ForestPlot(response.dat[response.dat$x == "Change in real estate value, 1860-1870 (1860$)" | 
                           response.dat$x == "Change in personal property value, 1860-1870 (1860$)" |
                           response.dat$x == "Change in total census wealth, 1860-1870 (1860$)",],
           xlab="RD estimate",ylab="") +
  scale_y_continuous(breaks = c(-10000,0,20000), labels = c("-10,000", "0", "20,000")) + 
  facet_grid(.~Bandwidth)
dev.off() 
