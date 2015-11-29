## Exploratory data analysis: 1850 and 1860 censuses

library(ggplot2)
library(xtable)
library(reshape)

# Make data for histogram
wealth.dens.plot <- melt(data=data.frame("Census"=c(rep("1850 100%",nrow(ipums.50)), 
                                                    rep("1860 1%",nrow(ipums.60.1))),
                                         "realprop"= c(ipums.50$realprop,
                                                       ipums.60.1$realprop)), 
                         id.vars="Census") 

# Make density plot for real property
wealth.dens <- ggplot(wealth.dens.plot, aes(x = value, fill=Census)) + 
  geom_density(alpha=.3) + 
  xlim(c(1,30000)) +
  ylab("Density") +
  xlab("Value of real property owned") +
  geom_vline(xintercept = 20000, colour="red", linetype = "longdash")

ggsave(paste0(data.directory,"wealth-dens.png"), wealth.dens, width=11, height=8.5)

# Create summary statistics table.
ipums.s$literate <- ifelse(ipums.s$LIT==4,1,0) # create variables
ipums.s$school <- ifelse(ipums.s$SCHOOL==2,1,0)
ipums.s <- cbind(ipums.s,dummify(as.factor(ipums.s$OCC)))

my.stats <- list("n", "min", "mean", "max", "s") # create table
tableContinuous(vars =ipums.s[c("surname.length","surname.freq","AGE","REALPROP","literate","school","5","22","39","41","49","54","97","136","157","203","266")], prec = 3,stats=my.stats,cap = "`Surname length' is the character length of surnames. `Surname frequency' is the number of times surnames appear in the sample. `Literate' is a binary variable indicating literacy (can read and write). `In school' is an indicator variable for individuals currently in school. Sample is drawn from the 1850 full--count Census. The occupations dummies indicate contemporary occupational categories. Sample is restricted to male heads of households aged 21 and over who living in Georgia at the time of the census, were born in Georgia, and have non--missing surnames and property value.", lab = "sum-1850")


# What % in thirteenth exception?
sum(ipums.50$realprop >= 20000) /nrow(ipums.50)
sum(ipums.60.1$realprop >= 20000) /nrow(ipums.60.1)

# Calc property wealth deciles
wealth.dec.50 <- subset(ipums.50, select=c("serial","realprop")) %>%
  mutate(quantile = ntile(realprop, 10))

wealth.dec.60 <- subset(ipums.60.1, select=c("serial","realprop")) %>%
  mutate(quantile = ntile(realprop, 10))

sum(wealth.dec.50$realprop[wealth.dec.50$quantile==10])/sum(wealth.dec.50$realprop)

sum(wealth.dec.60$realprop[wealth.dec.60$quantile==10])/sum(wealth.dec.60$realprop)

head(sort(table(ipums.60.1$occstr[which(wealth.dec.60$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth

head(sort(table(ipums.50$occstr[which(wealth.dec.50$quantile==10)]), decreasing = TRUE)) # see which occupations are in top decile of wealth
