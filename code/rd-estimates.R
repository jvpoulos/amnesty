########################
### RD estimates     ###
########################

# Summary figure for estimates
ForestPlot <- function(d, xlab, ylab){
  # Forest plot for summary figure
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=Analysis)) + 
    geom_pointrange(size=1, alpha=0.5) + 
    coord_flip() +
    geom_hline(aes(x=0), lty=2) +
    theme(legend.position="top") +
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

ik.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="IK",
                                                          kernel="uniform")) 

cv.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV",
                                                          kernel="uniform")) 


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
                                 sapply(cct.response.srd, "[[", "coef")[3,]/beta,
                                 sapply(ik.response.srd, "[[", "coef")[1,],  # IK: conventional estimates
                                 sapply(ik.response.srd, "[[", "coef")[1,]/beta,
                                 sapply(cv.response.srd, "[[", "coef")[1,], # CV: conventional estimates
                                 sapply(cv.response.srd, "[[", "coef")[1,]/beta),
                           y.lo = c(sapply(cct.response.srd, "[[", "ci")[3,], # CCT: robust CIs
                                    sapply(cct.response.srd, "[[", "ci")[3,]/beta,
                                    sapply(ik.response.srd, "[[", "ci")[1,], # IK: conventional CIs
                                    sapply(ik.response.srd, "[[", "ci")[1,]/beta,
                                    sapply(cv.response.srd, "[[", "ci")[1,], # CV: conventional CIs
                                    sapply(cv.response.srd, "[[", "ci")[1,]/beta),
                           y.hi = c(sapply(cct.response.srd, "[[", "ci")[5,],
                                    sapply(cct.response.srd, "[[", "ci")[5,]/beta,
                                    sapply(ik.response.srd, "[[", "ci")[4,],
                                    sapply(ik.response.srd, "[[", "ci")[4,]/beta,
                                    sapply(cv.response.srd, "[[", "ci")[4,],
                                    sapply(cv.response.srd, "[[", "ci")[4,]/beta))

response.dat$Method <- c(rep("CCT",length(response.vars), each=2),
                         rep("IK",length(response.vars), each=2),
                         rep("CV",length(response.vars), each=2))

response.dat$Analysis <- rep(rep(c("ITT","TOT"),each=length(response.vars)),3)

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
  scale_y_continuous(breaks = c(-10000,0,10000,20000,30000), labels = c("-10,000","0","10,000","20,000","30,000")) + 
  facet_grid(.~Method)
dev.off() 
