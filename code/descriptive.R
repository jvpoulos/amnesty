#####################################
### Descriptive statistics         ###
#####################################

# Make density plot for real property
wealth.dens <- melt(data=data.frame("Sample"=c(rep("1850 100%",nrow(ipums.50)),
                                               rep("1850 slaveholders",nrow(slave.50)),
                                               rep("1860 1%",nrow(ipums.60.1)),
                                               rep("1860 delegates",nrow(delegates)),
                                               rep("1860 slaveholders",nrow(slave.60))),
                                    "realprop"= c(ipums.50$realprop,
                                                  rep(NA,nrow(slave.50)),
                                                  ipums.60.1$realprop,
                                                  delegates$realprop.60,
                                                  rep(NA,nrow(slave.60))), 
                                    "persprop"= c(rep(NA,nrow(ipums.50)),
                                                  slave.50$slave.value,
                                                  ipums.60.1$persprop,
                                                  delegates$persprop.60,
                                                  slave.60$slave.value), 
                                    "taxprop"= c(rep(NA,nrow(ipums.50)),
                                                 rep(NA,nrow(slave.50)),
                                                 ipums.60.1$taxprop,
                                                 delegates$taxprop.60,
                                                 rep(NA,nrow(slave.60))), 
                                    id.vars="Sample"))

realprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="realprop",], 
                        aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Real estate value (1860$)") 

persprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="persprop",], 
                        aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Personal property value (1860$)") 

taxprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="taxprop",], 
                       aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Taxable property value (1860$)") +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash")

ggsave(paste0(data.directory,"plots/taxprop-plot.pdf"), taxprop.plot + ylab("Scaled density"), width=8.5, height=11)

# Combine plots
pdf(paste0(data.directory,"plots/wealth-plots.pdf"), width=8.27, height=11.69)
print(grid.arrange(realprop.plot, persprop.plot,taxprop.plot,
                   ncol=1, nrow=3, left="Scaled density", bottom="")) 
dev.off() 

# Create summary table for delegates
my.stats <- list("n", "min", "median", "mean", "max", 
                 "s", "na") # create table
tableContinuous(vars =delegates[c("realprop.60","persprop.60","taxprop.60","realprop.70","persprop.70","taxprop.70",
                                  pretreat.vars,response.vars)], 
                prec = 2,stats=my.stats, lab = "delegates-sum")


# Create estimates for data table

#What % in thirteenth exception?
sum(ipums.60.1$taxprop >= 20000) /nrow(ipums.60.1) #1860 1%
sum(delegates$taxprop.60 >= 20000, na.rm=TRUE) /nrow(delegates) #1860 delegates