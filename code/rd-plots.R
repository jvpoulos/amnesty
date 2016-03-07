# Create RD plots

require(rdrobust)

source("delegates.R") # Run delegates first

options(scipen=999) # turn of scientific notation

# Subset data to nonmissing taxable property values 
delegates.rd <- subset(delegates, !is.na(taxprop.60))

# Data-driven plots
cutoff <- 20000 # define cutoff
upper <- 2*cutoff # define upper margin

RdPlot <- function(data=delegates.rd,y.var,x.var="taxprop.60",xlab="",poly=3,
                   ylab,upperend=upper,ylim=NULL,continuous=TRUE){
  # Data-driven regression discontinuity plots
  if(continuous){
  rdplot(y=data[,y.var],
         x=data[,x.var],
         c=cutoff,
         p=poly, 
         x.label=xlab,
         y.label=ylab,
         upperend=upper,
         y.lim=ylim,
         title="",
         xaxt="n",
         yaxt="n",
         cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
   box(bty='L')
   axis(side=1, at=axTicks(1), labels=formatC(axTicks(1), format="d", big.mark=','))
   axis(side=2, at=axTicks(2), labels=formatC(axTicks(2), format="d", big.mark=','))
   text(x=10,y=-upper, paste("N=", nrow(data[!is.na(data[,y.var]),]), sep=""),cex=1.5)
   } else{
     rdplot(y=data[,y.var],
            x=data[,x.var],
            c=cutoff,
            p=poly,
            x.label=xlab,
            y.label=ylab,
            upperend=upper,
            y.lim=ylim,
            title="",
            xaxt="n",
            cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
     box(bty='L')
     axis(side=1, at=axTicks(1), labels=formatC(axTicks(1), format="d", big.mark=','))
     text(x=10,y=0, paste("N=", nrow(data[!is.na(data[,y.var]),]), sep=""),cex=1.5)
  }
}

pdf(paste0(data.directory,"plots/realprop_d.pdf"), width=11.69, height=8.27)
RdPlot(y.var="realprop.d",
       ylab="Change in real estate value, 1860-1870 (1860$)",
       ylim = c(-upper,upper)) 
dev.off() 

pdf(paste0(data.directory,"plots/persprop_d.pdf"), width=11.69, height=8.27)
RdPlot(y.var="persprop.d",
       ylab="Change in personal property value, 1860-1870 (1860$)",
       ylim = c(-upper,upper)) 
dev.off() 

# pdf(paste0(data.directory,"plots/taxprop_d.pdf"), width=11.69, height=8.27)
# RdPlot(y.var="taxprop.d",
#        ylab="Change in taxable property value, 1860-1870 (1860$)",
#        ylim = c(-upper,upper)) 
# dev.off() 

pdf(paste0(data.directory,"plots/future.pdf"), width=11.69, height=8.27)
RdPlot(y.var="future",
       ylab="Future officeholder",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/overall.pdf"), width=11.69, height=8.27)
RdPlot(y.var="overall",
       ylab="Overall",
       ylim= c(0,5),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/race.pdf"), width=11.69, height=8.27)
RdPlot(y.var="race",
       ylab="Race",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/misc.pdf"), width=11.69, height=8.27)
RdPlot(y.var="misc",
       ylab="Misc.",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/gov.pdf"), width=11.69, height=8.27)
RdPlot(y.var="gov",
       ylab="Gov. structure",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/suffrage.pdf"), width=11.69, height=8.27)
RdPlot(y.var="suffrage",
       ylab="Suffrage",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/econ.pdf"), width=11.69, height=8.27)
RdPlot(y.var="econ",
       ylab="Economic",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 

pdf(paste0(data.directory,"plots/protest.pdf"), width=11.69, height=8.27)
RdPlot(y.var="protest",
       ylab="Protested adoption of state constitution",
       ylim= c(0,1),
       continuous=FALSE)
dev.off() 