########################
### RD balance plots ###
########################

# Create vector for pretreatment variables
pretreat.vars <- c("age","confederate","dem","former","unionist",
                   "per.black",
                   "farmer","lawyer","merchant","physician")

# Apply rdrobust over characteristics

cct.pretreat.srd <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                               delegates.rd$taxprop.60,
                                                               c=cutoff,
                                                               all=TRUE,
                                                               bwselect="CCT",
                                                               kernel="uniform")) 

ik.pretreat.srd <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                              delegates.rd$taxprop.60,
                                                              c=cutoff,
                                                              all=TRUE,
                                                              bwselect="IK",
                                                              kernel="uniform")) 

cv.pretreat.srd <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                              delegates.rd$taxprop.60,
                                                              c=cutoff,
                                                              all=TRUE,
                                                              bwselect="CV",
                                                              kernel="uniform")) 

# Create function for plot theme
ThemeBw1 <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
      legend.position = "top"
    )
}

# Create balance plot data
covars.names <- c("Age","Confederate","Democrat","Former officeholder","Unionist",
                  "Percent black",
                  "Farmer","Lawyer","Merchant","Physician")

covars <- data.frame("covars"=covars.names,
                     "p.cct.srd"=sapply(cct.pretreat.srd, "[[", "pv")[3,], # CCT: robust 
                     "p.ik.srd"= sapply(ik.pretreat.srd, "[[", "pv")[1,],  # IK: conventional 
                     "p.cv.srd"= sapply(cv.pretreat.srd, "[[", "pv")[1,])  # CV: conventional 

Biographical  <- covars.names[1:5] # group vars
District    <- covars.names[6]
Occupations       <- covars.names[7:10]

covars$group <- NA
covars$group[covars$covars %in% Biographical]       <- "Biographical"
covars$group[covars$covars %in% District]       <- "District characteristics"
covars$group[covars$covars %in% Occupations]       <- "Occupations"

offset <- c("   ")
covars$covars <- paste(offset,covars$covars)

covars$order <- 1:nrow(covars)  # reorder  
order <- data.frame(covars= c("Biographical:",
                              "  ",
                              "District characteristics:",
                              "   ",
                              "Occupations:"),
                     order=c(.5,5.1,5.5,6.1,6.5), p.cct.srd=NA, p.ik.srd=NA,p.cv.srd=NA,group=NA)
                   # order=c(.5,5.1,5.5,6.1,6.5), p.cct.srd=NA, p.cct.frd=NA, group=NA)
covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars,aes(y=p.cct.srd,x=covars)) +  
  coord_flip(ylim = c(0, 1)) + 
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  scale_colour_manual(name="Estimator/bandwidth selector", values =c("CV"="orange", "IK"="blue", "CCT"="red")) +
  #scale_colour_manual(name="Estimator", values =c("SRD"="orange", "FRD"="blue")) +
  geom_point(aes(colour ='CCT'),size=2) + 
  geom_point(aes(y=p.ik.srd,x=covars,colour ='IK'), size=2) +
  geom_point(aes(y=p.cv.srd,x=covars,colour ='CV'), size=2) +
  #geom_point(aes(colour ='SRD'),size=2) + 
  #geom_point(aes(y=p.cct.frd,x=covars,colour ='FRD'), size=2) +
  scale_y_continuous(name="SRD p-value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"plots/balance-plot.pdf"), p, width=8.27, height=11.69)

### RD plots for pretreatment variables ###

pdf(paste0(data.directory,"plots/age.pdf"), width=11.69, height=8.27)
RdPlot(y.var="age",
       ylab="Age",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/confederate.pdf"), width=11.69, height=8.27)
RdPlot(y.var="confederate",
       ylab="Confederate",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/dem.pdf"), width=11.69, height=8.27)
RdPlot(y.var="dem",
       ylab="Democrat",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/farmer.pdf"), width=11.69, height=8.27)
RdPlot(y.var="farmer",
       ylab="Farmer",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/former.pdf"), width=11.69, height=8.27)
RdPlot(y.var="former",
       ylab="Former officeholder",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/lawyer.pdf"), width=11.69, height=8.27)
RdPlot(y.var="lawyer",
       ylab="Lawyer",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/merchant.pdf"), width=11.69, height=8.27)
RdPlot(y.var="merchant",
       ylab="Merchant",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/per_black.pdf"), width=11.69, height=8.27)
RdPlot(y.var="per.black",
       ylab="Percent black",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/physician.pdf"), width=11.69, height=8.27)
RdPlot(y.var="physician",
       ylab="Physician",
       ylim= c(0,1))
dev.off() 

pdf(paste0(data.directory,"plots/unionist.pdf"), width=11.69, height=8.27)
RdPlot(y.var="unionist",
       ylab="Unionist",
       ylim= c(0,1))
dev.off() 

### Density of the forcing variable plot ###

# Source functions
source(paste0(code.directory,"rddensity/rddensity.R")) # http://www-personal.umich.edu/~cattaneo/software/rddensity/R/

# Apply manipulation test over range of cutoff values
cutoff.range <- quantile(log(delegates.rd$taxprop.60), seq(0.15,0.95,0.025)) # put in log

dens.range <- lapply(cutoff.range, function(x) rddensity(X = log(delegates.rd$taxprop.60[delegates.rd$taxprop.60>0]), 
                                                         c=x, 
                                                         vce="jackknife", 
                                                         print.screen=FALSE))
# Create data for plot
dens.dat <- data.frame(cutoff = quantile(delegates.rd$taxprop.60, seq(0.15,0.95,0.025)), # no log for labels
                       y = unlist(sapply(dens.range, "[[", "test")[4,]))

# Plot p values
dens.plot <- ggplot(dens.dat, aes(x=cutoff, y=y)) + 
  geom_point(size=3, alpha=0.8) + 
  geom_hline(aes(x=0), lty=2) +
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  geom_vline(xintercept = cutoff,size=.5,colour="red",linetype="dashed") +
  coord_flip() +
  scale_x_continuous(name="Value of total census wealth used as cutoff (1860$)",breaks=c(0,cutoff,cutoff*2,cutoff*3),labels=c("0","20,000","30,000","40,000")) +
  scale_y_continuous(name="SRD p-value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) 

ggsave(paste0(data.directory,"plots/density-plot.pdf"), dens.plot, width=8.27, height=11.69)