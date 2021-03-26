########################
### RD balance plots ###
########################

# Create vector for pretreatment variables

slaveholders.60.rd <- cbind(slaveholders.60.rd, dummify(as.factor(slaveholders.60.rd$state)))
pretreat.vars <- c("self_residence_info_age", "Alabama", "Georgia", "Mississippi", "South Carolina", "farmer","lawyer","merchant","physician","minister","other")

# Apply rdrobust over characteristics

mserd.pretreat.srd <- lapply(pretreat.vars, function(i) rdrobust(slaveholders.60.rd[,i], 
                                                               slaveholders.60.rd$taxprop,
                                                               c=cutoff)) 

# Create balance plot data
covars.names <- c("Age",
                  "Alabama", "Georgia", "Mississippi", "South Carolina",
                  "Farmer","Lawyer","Merchant","Physician","Minister","Other")

covars <- data.frame("covars"=covars.names,
                     "p.mserd.srd"=sapply(mserd.pretreat.srd, "[[", "pv")[3,])  # bias-corrected estimates with robust standard errors

Biographical  <- covars.names[1] # group vars
District    <- covars.names[2:5]
Occupations       <- covars.names[6:11]

covars$group <- NA
covars$group[covars$covars %in% Biographical]       <- "Biographical"
covars$group[covars$covars %in% District]       <- "State of residence"
covars$group[covars$covars %in% Occupations]       <- "Occupation"

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
order <- data.frame(covars= c("Biographical:",
                              "  ",
                              "State of residence:",
                              "   ",
                              "Occupations:"),
                     order=c(.5,1.1,1.5,5.1,5.5), p.mserd.srd=NA, group=NA)

covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars,aes(y=p.mserd.srd,x=covars)) +  
  coord_flip(ylim = c(0, 1)) + 
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  geom_point(size=2) + 
  scale_y_continuous(name="p-value",breaks=c(0,0.05,1),labels=c("0","0.05","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1() + ggtitle(paste0("1860 slaveholders, N=",format(nrow(slaveholders.60.rd), nsmall=0, big.mark=","))) + theme(plot.title = element_text(hjust = 0.5))

ggsave("data/plots/balance-plot-slaveholders.png", p, scale=1.25)

### Density of the forcing variable plot ###

# Source functions
source("code/rddensity/rddensity.R") # http://www-personal.umich.edu/~cattaneo/software/rddensity/R/

# Apply manipulation test over range of cutoff values
cutoff.range <- quantile(log1p(slaveholders.60.rd$taxprop), seq(0.15,0.95,0.05)) # put in log

dens.range <- lapply(cutoff.range, function(x) rddensity(X = log1p(slaveholders.60.rd$taxprop), 
                                                         c=x, 
                                                         vce="jackknife", 
                                                         print.screen=FALSE))
# Create data for plot
dens.dat <- data.frame(cutoff = quantile(slaveholders.60.rd$taxprop, seq(0.15,0.95,0.05)), # no log for labels
                       y = unlist(sapply(dens.range, "[[", "test")[4,]))

# Plot p values
dens.plot <- ggplot(dens.dat, aes(x=cutoff, y=y)) + 
  geom_point(size=3, alpha=0.8) + 
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  geom_vline(xintercept = cutoff,size=.5,colour="red",linetype="dashed") +
  coord_flip() +
  scale_x_continuous(name="Value of total census wealth used as cutoff (1860$)",breaks=c(0,cutoff/2,cutoff,cutoff*2,cutoff*3),labels=c("0","10,000","20,000","30,000","40,000")) +
  scale_y_continuous(name="p-value",breaks=c(0,0.05,1),labels=c("0","0.05","1")) + ggtitle(paste0("1860 slaveholders, N=",format(nrow(slaveholders.60.rd), nsmall=0, big.mark=","))) + theme(plot.title = element_text(hjust = 0.5))

ggsave("data/plots/density-plot-slaveholders.png", dens.plot, scale=1.25)