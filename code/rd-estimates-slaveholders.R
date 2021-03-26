########################
### RD estimates     ###
########################

# Create vector for responsement variables
response.vars <- c("Personal Estate 1870","Real Estate 1870","taxprop.1870") # ,"persprop.d","realprop.d","taxprop.d"

# Apply rdrobust over responses

mserd.response.srd <- lapply(response.vars, function(i) rdrobust(slaveholders.60.rd[,i], 
                                                               slaveholders.60.rd$taxprop,
                                                               c=cutoff)) 

# Create data for plot
response.dat <- data.frame(x = c(paste0("Personal property value,\n N=",format(as.numeric(nrow(na.omit(slaveholders.60.rd[c("Personal Estate 1870")]))), nsmall=0, big.mark=",")),
                                 paste0("Real estate value,\n N=",format(as.numeric(nrow(na.omit(slaveholders.60.rd[c("Real Estate 1870")]))), nsmall=0, big.mark=",")),
                                 paste0("Total census wealth,\n N=",format(as.numeric(nrow(na.omit(slaveholders.60.rd[c("taxprop.1870")]))), nsmall=0, big.mark=","))),
                           y = c(sapply(mserd.response.srd, "[[", "coef")[3,], # mserd: robust estimates
                                 sapply(mserd.response.srd, "[[", "coef")[3,]/slaveholders.60.beta),
                           y.lo = c(sapply(mserd.response.srd, "[[", "ci")[3,], # mserd: robust CIs
                                    sapply(mserd.response.srd, "[[", "ci")[3,]/slaveholders.60.beta),
                           y.hi = c(sapply(mserd.response.srd, "[[", "ci")[5,],
                                    sapply(mserd.response.srd, "[[", "ci")[5,]/slaveholders.60.beta))

response.dat$x <- as.factor(response.dat$x)

response.dat$Analysis <- rep(rep(c("ITT","TOT"),each=length(response.vars)),1)

# Plot forest plots

ForestPlot(response.dat,
           xlab="Treatment effect estimate",ylab="1870 census wealth (1860$)") +
  scale_x_discrete(limits = rev(levels(response.dat$x))) +
  scale_y_continuous(breaks = c(-20000,0,20000,40000,60000), labels = c("-20,000","0","20,000","40,000","60,000")) +
  ggtitle("1860 slaveholders in the 1870 Census") + theme(plot.title = element_text(hjust = 0.5))

ggsave("data/plots/rd_estimates_wealth_slaveholders.png", plot=last_plot(), scale=1.25)
