#####################################
### Power analysis (county-level)  ###
#####################################

# Define simulation parameters
alpha <- 0.05
L <- 1000 # no. iterations 
delta <- seq(0.1,0.7,0.1) # effect size for continuous 

# Create grid for parameters
grid.cont <- expand.grid("delta"=delta)

# Define parameters
slave.60$taxprop <- slave.60$n.slaves*1800 # proxy by no. slaves times value of prime field-hands in GA in 1860 (Philips 1905)
slave.60 <- merge(slave.60, census.county.1860[c("County","State","wmtot")], by.x=c("county","state"), by.y=c("County","State"), all.x=TRUE)

cutoff <- 20000 # define cutoff
county.df <- ddply(slave.60, .(county,state), summarize,  excepted=sum(taxprop>=cutoff), wmtot=mean(wmtot))

county.df$excepted.p <- county.df$excepted/county.df$wmtot

county.df$beta <- rbinom(nrow(county.df), county.df$excepted, 0.9)/county.df$wmtot
county.df$beta[is.na(county.df$beta)] <- 0

county.df <- county.df[!is.na(county.df$excepted.p),] # remove LA county with missing pop. total

p.vals.cont <- replicate(L,
                           sapply(1:nrow(grid.cont), function(i){
                             SimRegression(delta[i], county.df)}))

p.vals.cont.array <- t(sapply(1:L, function(i) array(p.vals.cont[,i])))  # rows: iterations
saveRDS(p.vals.cont.array, "power_p_values_cont_county.rds")
# p.vals.wealth.array <- readRDS(paste0(data.directory,"power_p_values_wealth_fixed.rds"))

grid.cont$power <- apply(p.vals.cont.array, 2, function (x) length(which(x < alpha))/L)

# Fixed sample size
# Create plots
power.plot.cont <- ggplot(data=grid.cont, aes(x=delta, 
                                                  y=power)) +
  geom_line() +
  scale_x_continuous(breaks=delta, labels = c("10%","20%","30%","40%","50","60%","70%")) +
  scale_y_continuous(breaks=c(0.25,0.50,0.75,0.8,1), labels = c("25%", "50%", "75%","80%","100%")) +
  geom_hline(yintercept = 0.8, colour="black", linetype = "longdash") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Power") +
  xlab("Effect Size") +
  ggtitle("Continuous county-response (N=763)")

ggsave(paste0(data.directory,"plots/power-county.pdf"), power.plot.cont, width=8.5, height=11) 
