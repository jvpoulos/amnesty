########################
### RD falsification ###
########################

# Create range of cutoff values 
cutoff.range <- seq(500,35000,500)

# Apply rdrobust over range of cutoff values
range.realprop <- lapply(cutoff.range, function(x) rdrobust(delegates.rd$realprop.d, 
                                                            delegates.rd$taxprop.60,
                                                            c=x,
                                                            all=TRUE,
                                                            bwselect="CCT",
                                                            kernel="uniform"))

range.future <- lapply(cutoff.range, function(x) rdrobust(delegates.rd$future, 
                                                          delegates.rd$taxprop.60,
                                                          c=x,
                                                          all=TRUE,
                                                          bwselect="CCT",
                                                          kernel="uniform"))
# Create data for plot
realprop.dat <- data.frame(cutoff = cutoff.range,
                           y = sapply(range.realprop, "[[", "coef")[3,], # CCT: robust estimates
                           y.lo = sapply(range.realprop, "[[", "ci")[3,], # CCT: robust CIs
                           y.hi = sapply(range.realprop, "[[", "ci")[6,],
                           N = sapply(range.realprop, "[[", "N"))

future.dat <- data.frame(cutoff = cutoff.range,
                         y = sapply(range.future, "[[", "coef")[3,], # CCT: robust estimates
                         y.lo = sapply(range.future, "[[", "ci")[3,], # CCT: robust CIs
                         y.hi = sapply(range.future, "[[", "ci")[6,],
                         N = sapply(range.future, "[[", "N"))

# Plot estimates
realprop.d.plot <- ggplot(realprop.dat, aes(x=cutoff, y=y, ymin=y.lo, ymax=y.hi)) + 
  geom_pointrange(size=1, alpha=0.8) + 
  geom_hline(aes(x=0), lty=2) +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash") +
  scale_y_continuous(breaks = c(-40000,0,40000), labels = c("-40,000", "0", "40,000")) +
  scale_x_continuous(breaks = c(0,10000,20000,30000), labels = c("0", "10,000", "20,000", "30,000")) +
  ylab("Change in real estate value, 1860-1870 (1860$)") +
  xlab("")

future.plot <- ggplot(future.dat, aes(x=cutoff, y=y, ymin=y.lo, ymax=y.hi)) + 
  geom_pointrange(size=1, alpha=0.8) + 
  geom_hline(aes(x=0), lty=2) +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash") +
  scale_x_continuous(breaks = c(0,10000,20000,30000), labels = c("0", "10,000", "20,000", "30,000")) +
  ylab("Ex-post officeholder") +
  xlab("")

pdf(paste0(data.directory,"plots/false-plots.pdf"), width=8.27, height=11.69)
print(grid.arrange(realprop.d.plot, future.plot,
                   ncol=1, nrow=2, left="RD Estimate", bottom="Value of 1860 total census wealth used as cutoff")) 
dev.off() 
