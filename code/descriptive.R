#####################################
### Descriptive statistics         ###
#####################################

# Expand 1860 wealth vars by weight
ipums.60.ex <- untable(ipums.60.1, num=ipums.60.1[,"perwt"])

## Make density plot for real property
wealth.dens <- melt(data=data.frame("Sample"=c(#rep("1850 100%",nrow(ipums.50)),
                          #                     rep("1850 slaveholders",nrow(slave.50)),
                                               rep("1860 1%",nrow(ipums.60.ex)),
                                               rep("1860 delegates",nrow(delegates)),
                                               rep("1860 slaveholders",nrow(slave.60))),
                                    "realprop"= c(#ipums.50$realprop,
                         #                         rep(NA,nrow(slave.50)),
                                                  ipums.60.ex$realprop,
                                                  delegates$realprop.60,
                                                  rep(NA,nrow(slave.60))), 
                                    "persprop"= c(#rep(NA,nrow(ipums.50)),
                          #                        slave.50$slave.value,
                                                  ipums.60.ex$persprop,
                                                  delegates$persprop.60,
                                                  slave.60$slave.value), 
                                    "taxprop"= c(#rep(NA,nrow(ipums.50)),
                            #                     rep(NA,nrow(slave.50)),
                                                 ipums.60.ex$taxprop,
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
  xlab("Total census wealth (1860$)") +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash")

ggsave(paste0(data.directory,"plots/taxprop-plot.pdf"), taxprop.plot + ylab("Scaled density"), width=8.5, height=11)

# Combine plots
pdf(paste0(data.directory,"plots/wealth-plots.pdf"), width=8.27, height=11.69)
print(grid.arrange(realprop.plot, persprop.plot,taxprop.plot,
                   ncol=1, nrow=3, left="Scaled density", bottom="")) 
dev.off() 

## Create summary table for delegates
pretreat.vars <- c("age","confederate","dem","former","unionist",
                   "per.black",
                   "farmer","lawyer","merchant","physician")

response.vars <- c("persprop.70","realprop.70","taxprop.70","future","protest","overall","econ","gov","misc","race","suffrage")

my.stats <- list("n", "min", "median", "mean", "max", 
                 "s", "na") # create table
tableContinuous(vars =delegates[c("realprop.60","persprop.60","taxprop.60","realprop.70","persprop.70","taxprop.70",
                                  pretreat.vars,response.vars)], 
                prec = 2,stats=my.stats, lab = "delegates-sum")

## Descriptive plot: overall RSS vs 1860 taxable property

pdf(paste0(data.directory,"plots/rss-wealth.pdf"), width=11.69, height=8.27)
ggplot(delegates, aes(y=overall, x=taxprop.60)) + 
  geom_point(shape=19, alpha=1/4) + 
  xlab("Total census wealth in 1860 (1860$)") + 
  ylab("Overall RSS") +
  #geom_vline(aes(xintercept=20000), colour="red", linetype = "longdash") + 
  stat_smooth(method = "lm", formula = y ~ poly(x, 4),se=FALSE) + 
  scale_x_continuous(limit=c(0,40000),labels = c("0", "10,000", "20,000", "30,000", "40,000"))
dev.off()

## Descriptive plot: binary background variables box plot

bin.melt <- melt(delegates[c("taxprop.60","former","unionist","dem","confederate")],
                  id.vars="taxprop.60")

pdf(paste0(data.directory,"plots/bin-wealth.pdf"), width=11.69, height=8.27)
ggplot(data=bin.melt,aes(x=value,y= taxprop.60,colour=variable))+
  scale_y_continuous(limit=c(0,40000),labels = c("0", "10,000", "20,000", "30,000", "40,000")) +
  scale_x_continuous(breaks=c(0,1), labels=c("No","Yes")) +
  geom_boxplot(aes(group=value)) +
  facet_wrap(~variable,  nrow=1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  #theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
  labs(y="Total census wealth in 1860 (1860$)",x="") +
  scale_color_discrete("Pretreatment variable",
                       labels=c("Former officeholder", "Unionist", "Democrat", "Confederate"))
dev.off()

## Create stacked bar plot for 13th exception state totals

ipums.60.ex$thr <- ifelse(ipums.60.ex$taxprop >= 20000, 1, 0) # create dummy for thirteenth exception

# Create data for table
exception.dat <- data.frame("State"= c("VA","GA","NC","TX","MS","LA","TN","AL","SC","AR","FL"),
                            "Pardons"=c(2070,1228,482,269,765,142,93,1361,638,41,22),
                            "Total"=c(round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Virginia"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Virginia",])*nrow(ipums.60[ipums.60$state=="Virginia",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Georgia"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Georgia",])*nrow(ipums.60[ipums.60$state=="Georgia",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="North Carolina"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="North Carolina",])*nrow(ipums.60[ipums.60$state=="North Carolina",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Texas"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Texas",])*nrow(ipums.60[ipums.60$state=="Texas",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Mississippi"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Mississippi",])*nrow(ipums.60[ipums.60$state=="Mississippi",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Louisiana"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Louisiana",])*nrow(ipums.60[ipums.60$state=="Louisiana",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Tennessee"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Tennessee",])*nrow(ipums.60[ipums.60$state=="Tennessee",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Alabama"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Alabama",])*nrow(ipums.60[ipums.60$state=="Alabama",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="South Carolina"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="South Carolina",])*nrow(ipums.60[ipums.60$state=="South Carolina",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Arkansas"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Arkansas",])*nrow(ipums.60[ipums.60$state=="Arkansas",])),
                                      round(sum(ipums.60.ex$thr[ipums.60.ex$statefip=="Florida"])/nrow(ipums.60.ex[ipums.60.ex$statefip=="Florida",])*nrow(ipums.60[ipums.60$state=="Florida",]))))

#exception.dat$Pardons[exception.dat$State=="CSA"] <- sum(exception.dat$Pardons,na.rm=TRUE)
#exception.dat$Est[exception.dat$State=="CSA"] <- sum(exception.dat$Est,na.rm=TRUE)                                

exception.dat.m <- melt(exception.dat, id.vars = "State")

colnames(exception.dat.m) <- c("State","Variable","value")

pdf(paste0(data.directory,"plots/pardon-plot.pdf"), width=11.69, height=8.27)
ggplot(exception.dat.m, aes(State, value, fill = Variable)) +
  scale_y_continuous(labels = c("0", "10,000", "20,000", "30,000", "40,000")) +
  geom_bar(stat="identity") +
  geom_text(aes(label = value), size = 3, hjust = 0.5, vjust = 3, position =     "stack") +
  xlab("State") +
  ylab("Count")
dev.off()


# What % in thirteenth exception?
sum(ipums.60.ex$thr==1) /nrow(ipums.60.ex) #1860 1%
sum(delegates.rd$treat) /nrow(delegates.rd) #1860 delegates

60000/nrow(ipums.60)
80000/nrow(ipums.60)

150000/nrow(ipums.60)
200000/nrow(ipums.60)

12470/nrow(ipums.60[ipums.60$state=="Georgia",])
15000/nrow(ipums.60[ipums.60$state=="Georgia",])
20000/nrow(ipums.60[ipums.60$state=="Georgia",])