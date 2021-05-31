#####################################
### Descriptive statistics         ###
#####################################

## Make density plot for real property
wealth.dens <- melt(data=data.frame("Sample"=c(rep("Reconstruction delegates",nrow(delegates)),
                                               rep("1860 slaveholders",nrow(slaveholders.60))),
                                    "realprop"= c(delegates$realprop.60,
                                                  slaveholders.60$`Real Estate`), 
                                    "persprop"= c(delegates$persprop.60,
                                                  slaveholders.60$`Personal Estate`), 
                                    "taxprop"= c(delegates$taxprop.60,
                                                 slaveholders.60$taxprop),
                                    id.vars="Sample"))

realprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="realprop",], 
                        aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Real estate value in 1860 (1860$)")

persprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="persprop",], 
                        aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Personal property value in 1860 (1860$)") 

taxprop.plot <- ggplot(wealth.dens[wealth.dens$variable=="taxprop",], 
                       aes(x = value, y= ..scaled.., fill=Sample)) + 
  geom_density(alpha=.3) + 
  scale_x_continuous(limits = c(1,50000), labels = c("0", "10,000", "20,000", "30,000", "40,000", "50,000")) +
  ylab("") +
  xlab("Total census wealth in 1860 (1860$)")

# extract legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <-g_legend(taxprop.plot)

# Combine plots
ggsave("data/plots/wealth-plots.png", plot=grid.arrange(realprop.plot + theme(legend.position = "none"), 
                                                        persprop.plot + theme(legend.position = "none"),
                                                        taxprop.plot + theme(legend.position = "none"),
                                                        mylegend,
                                                        ncol=2, nrow=2, left="Scaled density", bottom="") , scale=1.25)

## Create summary table for delegates
pretreat.vars <- c("age","confederate","dem","former","unionist",
                   "per.black",
                   "farmer","lawyer","merchant","physician")

response.vars <- c("persprop.70","realprop.70","taxprop.70","persprop.d","realprop.d","taxprop.d","future","protest","overall")

my.stats <- list("n", "min", "median", "mean", "max", 
                 "s", "na") # create table
print(tableContinuous(vars =delegates[c("realprop.60","persprop.60","taxprop.60",pretreat.vars,response.vars)], 
                prec = 2,stats=my.stats, lab = "delegates-sum"))

## Create summary table for delegates
pretreat.vars <- c("self_residence_info_age", "Alabama", "Georgia", "Mississippi", "South Carolina", "farmer","lawyer","merchant","physician","minister","other")

response.vars <- c("Personal Estate 1870","Real Estate 1870","taxprop.1870","persprop.d","realprop.d","taxprop.d")

my.stats <- list("n", "min", "median", "mean", "max", 
                 "s", "na") # create table
print(tableContinuous(vars =slaveholders.60.rd[c("Real Estate","Personal Estate","taxprop",pretreat.vars,response.vars)], 
                prec = 2,stats=my.stats, lab = "slaveholders-sum"))

## Descriptive plot: overall RSS vs 1860 taxable property

ggplot(delegates, aes(y=overall, x=taxprop.60)) + 
  geom_point(shape=19, alpha=1/4) + 
  xlab("Total census wealth in 1860 (1860$)") + 
  ylab("Republican support score") +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4),se=TRUE) + 
  scale_x_continuous(limit=c(0,40000),labels = c("0", "10,000", "20,000", "30,000", "40,000"))
ggsave("data/plots/rss-wealth.png", plot=last_plot(), scale=1.25)

## Descriptive plot: binary background variables box plot

bin.melt <- melt(delegates[c("taxprop.60","former","unionist","dem","confederate")],
                  id.vars="taxprop.60")

ggplot(data=bin.melt,aes(x=value,y= taxprop.60,colour=variable))+
  scale_y_continuous(limit=c(0,40000),labels = c("0", "10,000", "20,000", "30,000", "40,000")) +
  scale_x_continuous(breaks=c(0,1), labels=c("No","Yes")) +
  geom_boxplot(aes(group=value)) +
  facet_wrap(~variable,  nrow=1) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank()) +
  labs(y="Total census wealth in 1860 (1860$)",x="") +
  scale_color_discrete("Pretreatment variable",
                       labels=c("Former officeholder", "Unionist", "Democrat", "Confederate"))
ggsave("data/plots/bin-wealth.png", plot=last_plot(), scale=1.25)

# What % in thirteenth exception?

slaveholders.60$thr <- ifelse(slaveholders.60$taxprop >= 20000, 1, 0) # create dummy for thirteenth exception

print(sum(slaveholders.60$thr==1) /nrow(slaveholders.60)) #1860 slaveholders
print(sum(delegates.rd$treat) /nrow(delegates.rd)) #1860 delegates