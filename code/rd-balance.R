# RD balance plots

require(ggplot2)
require(rdrobust)

source("delegates.R") # 1
source("rd-plots.R") # 2

# Create vector for pretreatment variables
pretreat.vars <- c("age","confederate","dem","former","unionist",
                   "per.black",
                   "farmer","lawyer","merchant","physician")

# Apply rdrobust over characteristics
cct.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                           delegates.rd$taxprop.60,
                                                           c=cutoff,
                                                           all=TRUE,
                                                           bwselect="CCT")) 

ik.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="IK")) 

cv.pretreat <- lapply(pretreat.vars, function(i) rdrobust(delegates.rd[,i], 
                                                          delegates.rd$taxprop.60,
                                                          c=cutoff,
                                                          all=TRUE,
                                                          bwselect="CV")) 

# Create function for plot theme
ThemeBw1 <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x =       element_text(size = base_size*.9, colour = "black",  hjust = .5 , vjust=1),
      axis.text.y =       element_text(size = base_size, colour = "black", hjust = 0 , vjust=.5 ), # changes position of X axis text
      axis.ticks =        element_blank(),
      axis.title.y =      element_text(size = base_size,angle=90,vjust=.01,hjust=.1),
     legend.position = "right"
    )
}

# Create balance plot data
covars.names <- c("Age","Confederate","Democrat","Former officeholder","Unionist",
                  "Percent black",
                  "Farmer","Lawyer","Merchant","Physician")

covars <- data.frame("covars"=covars.names,
                     "p.cct"=sapply(cct.pretreat, "[[", "pv")[2,], # CCT: bias-corrected 
                     "p.ik"= sapply(ik.pretreat, "[[", "pv")[1,],  # IK: conventional 
                     "p.cv"= sapply(cv.pretreat, "[[", "pv")[1,])  # CV: conventional 

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
                    order=c(.5,5.1,5.5,6.1,6.5), p.cct=NA, p.ik=NA,p.cv=NA,group=NA)
covars <- rbind(covars,order)
covars <-covars[order(covars$order),]
covars$covars <- factor(covars$covars,levels=unique(covars$covars)[length(covars$covars):1])

# Create plot 

p <- ggplot(covars,aes(y=p.cct,x=covars)) +  
  coord_flip(ylim = c(0, 1)) + 
  geom_hline(yintercept = 0.05,size=.5,colour="blue",linetype="dotted") +
  scale_colour_manual(name="Estimator", values =c("CV"="orange", "IK"="blue", "CCT"="red")) +
  geom_point(aes(colour ='CCT'),size=2) + 
  geom_point(aes(y=p.ik,x=covars,colour ='IK'), size=2) +
  geom_point(aes(y=p.cv,x=covars,colour ='CV'), size=2) +
  scale_y_continuous(name="p value",breaks=c(0,0.05,0.10,1),labels=c("0","0.05","0.10","1")) + 
  scale_x_discrete(name="") + 
  ThemeBw1()

ggsave(paste0(data.directory,"plots/balance-plot.pdf"), p, width=8.5, height=11)

#################

# RD plots for pretreatment variables

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

# Density of the forcing variable plot