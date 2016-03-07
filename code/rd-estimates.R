# RD estimates

require(ggplot2)
require(reporttools)
require(sandwich)
require(lmtest)
require(rdrobust)
require(scales)
require(reshape2)

#source("delegates.R") # Run delegates first

ForestPlot <- function(d, xlab, ylab){
  # Forest plot for summary figure
  p <- ggplot(d, aes(x=x, y=y, ymin=y.lo, ymax=y.hi,colour=x)) + 
    geom_pointrange(size=1, alpha=0.8) + 
    coord_flip() +
    geom_hline(aes(x=0), lty=2) +
    theme(legend.position="none") +
    ylab(xlab) +
    xlab(ylab) #switch because of the coord_flip() above
  return(p)
}