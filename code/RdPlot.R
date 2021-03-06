RdPlot <- function(data,y.var,x.var="taxprop.60",xlab="",poly=2, #2nd order polynomial
                   ylab,title,ylim=NULL,continuous=FALSE){
  # Data-driven regression discontinuity plots
  if(continuous){
    rdplot(y=data[,y.var],
           x=data[,x.var],
           c=cutoff,
           p=poly, 
           x.label=xlab,
           y.label=ylab,
           y.lim=ylim,
           title=paste(title, ", N=", nrow(data[!is.na(data[,y.var]),]), sep=""))
  } else{
    rdplot(y=data[,y.var],
           x=data[,x.var],
           c=cutoff,
           p=poly,
           x.label=xlab,
           y.label=ylab,
           y.lim=ylim,
           title=paste(title, ", N=", nrow(data[!is.na(data[,y.var]),]), sep=""))
  }
}