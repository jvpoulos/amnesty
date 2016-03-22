#####################################
### Exploratory data analysis     ###
#####################################

# Make density plot for real property
wealth.dens.plot <- melt(data=data.frame("Census"=c(rep("1850 100%",nrow(ipums.50)), 
                                                    rep("1860 1%",nrow(ipums.60.1))),
                                         "realprop"= c(ipums.50$realprop,
                                                       ipums.60.1$realprop)), 
                         id.vars="Census") 

wealth.dens <- ggplot(wealth.dens.plot, aes(x = value, fill=Census)) + 
  geom_density(alpha=.3) + 
  xlim(c(1,30000)) +
  ylab("Density") +
  xlab("Value of real property owned") +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash")

ggsave(paste0(data.directory,"wealth-dens.png"), wealth.dens, width=11, height=8.5)

# Create summary statistics table.
ipums.50 <- cbind(ipums.50,dummify(as.factor(ipums.50$occ)))

my.stats <- list("min", "mean", "max", "s") # create table
tableContinuous(vars =ipums.50[c("age", "realprop","5","22","39","41","48","49","54","65","97","110","136","142","154","157","159","188","197","200","203","205","252","266","310")], prec = 3,stats=my.stats, lab = "census-sum")
tableContinuous(vars =ipums.50.1[c("slaveholder","n.slaves")], weights=ipums.50.1$perwt,prec = 3,stats=my.stats, lab = "census-sum")

# What % in thirteenth exception?
sum(ipums.50$realprop >= 20000) /nrow(ipums.50)
sum(ipums.60.1$realprop >= 20000) /nrow(ipums.60.1)

sum((ipums.60.1$realprop + ipums.60.1$persprop) >= 20000) /nrow(ipums.60.1) # combine w personal prop

# Calc property wealth deciles
wealth.dec.50 <- subset(ipums.50, select=c("serial","realprop")) %>%
  mutate(quantile = ntile(realprop, 10))

wealth.dec.60 <- subset(ipums.60.1, select=c("serial","realprop")) %>%
  mutate(quantile = ntile(realprop, 10))

sum(wealth.dec.50$realprop[wealth.dec.50$quantile==10])/sum(wealth.dec.50$realprop)

sum(wealth.dec.60$realprop[wealth.dec.60$quantile==10])/sum(wealth.dec.60$realprop)

head(sort(table(ipums.60.1$occstr[which(wealth.dec.60$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth

head(sort(table(ipums.50$occstr[which(wealth.dec.50$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth
