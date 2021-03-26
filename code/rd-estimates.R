########################
### RD estimates     ###
########################

# Create vector for responsement variables
response.vars <- c("persprop.70","realprop.70","taxprop.70","future","protest","overall") # "persprop.d","realprop.d","taxprop.d",

# Apply rdrobust over responses

mserd.response.srd <- lapply(response.vars, function(i) rdrobust(delegates.rd[,i], 
                                                               delegates.rd$taxprop.60,
                                                               c=cutoff)) 

# Create data for plot
response.dat <- data.frame(x = c(paste0("Personal property value,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("persprop.70")]))), nsmall=0, big.mark=",")),
                                 paste0("Real estate value,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("realprop.70")]))), nsmall=0, big.mark=",")),
                                 paste0("Total census wealth,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("taxprop.70")]))), nsmall=0, big.mark=",")),
                                 paste0("Ex-post officeholder,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("future")]))), nsmall=0, big.mark=",")),
                                 paste0("Protested constitution,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("protest")]))), nsmall=0, big.mark=",")),
                                 paste0("Republican support score,\n N=",format(as.numeric(nrow(na.omit(delegates.rd[c("overall")]))), nsmall=0, big.mark=","))),
                           y = c(sapply(mserd.response.srd, "[[", "coef")[3,], # mserd: robust estimates
                                 sapply(mserd.response.srd, "[[", "coef")[3,]/beta),
                           y.lo = c(sapply(mserd.response.srd, "[[", "ci")[3,], # mserd: robust CIs
                                    sapply(mserd.response.srd, "[[", "ci")[3,]/beta),
                           y.hi = c(sapply(mserd.response.srd, "[[", "ci")[5,],
                                    sapply(mserd.response.srd, "[[", "ci")[5,]/beta))

response.dat$x <- as.factor(response.dat$x)

response.dat$Analysis <- rep(rep(c("ITT","TOT"),each=length(response.vars)),1)

# Plot forest plots

ForestPlot(response.dat[c(4:6,10:12),],
           xlab="Treatment effect estimate",ylab="") +
  scale_x_discrete(limits = rev(levels(drop.levels(response.dat$x[response.dat$x %in% response.dat[c(4:6,10:12),]$x])))) +
  scale_y_continuous(breaks = c(-1,0,1), labels = c("-1", "0", "1")) + 
  ggtitle("Reconstruction delegates") + theme(plot.title = element_text(hjust = 0.5))

ggsave("data/plots/rd_estimates_bin.png", plot=last_plot(), scale=1.25)

ForestPlot(response.dat[-c(4:6,10:12),],
           xlab="Treatment effect estimate",ylab="1870 census wealth (1860$)") +
  scale_x_discrete(limits = rev(levels(drop.levels(response.dat$x[response.dat$x %in% response.dat[-c(4:6,10:12),]$x])))) +
  scale_y_continuous(breaks = c(-10000,0,10000,20000,30000), labels = c("-10,000","0","10,000","20,000","30,000")) + 
  ggtitle("Reconstruction delegates") + theme(plot.title = element_text(hjust = 0.5))

ggsave("data/plots/rd_estimates_wealth.png", plot=last_plot(), scale=1.25)
