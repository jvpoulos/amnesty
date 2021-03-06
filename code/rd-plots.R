########################
### RD plots     ###
########################

options(scipen=999) # turn off scientific notation

source("code/RdPlot.R")

# Delegates sample

RdPlot(data=delegates.rd,
       y.var="realprop.70",
       ylab="1870 real estate value (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/realprop_70.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="persprop.70",
       ylab="1870 personal property value (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/persprop_70.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="taxprop.70",
       ylab="1870 taxable property value (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/taxprop_70.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="realprop.d",
       ylab="Change in real estate value, 1860-1870 (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/realprop_d.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="persprop.d",
       ylab="Change in personal property value, 1860-1870 (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/persprop_d.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="taxprop.d",
       ylab="Change in taxable property value, 1860-1870 (1860$)",
       title="Reconstruction delegates",
       continuous=TRUE) 
ggsave("data/plots/taxprop_d.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="future",
       ylab="Ex-post officeholder",
       title="Reconstruction delegates",
       ylim= c(0,1))
ggsave("data/plots/future.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="overall",
       ylab="Republican support score",
       title="Reconstruction delegates",
       ylim= c(0,1))
ggsave("data/plots/overall.png", plot=last_plot(), scale=1.25)

RdPlot(data=delegates.rd,
       y.var="protest",
       ylab="Protested constitution",
       title="Reconstruction delegates",
       ylim= c(0,1))
ggsave("data/plots/protest.png", plot=last_plot(), scale=1.25)

# 1860 slaveholders sample

RdPlot(data=slaveholders.60.rd,
       y.var="Real Estate 1870",
       x.var="taxprop",
       ylab="1870 real estate value (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/realprop_70_slaveholders.png", plot=last_plot(), scale=1.25)

RdPlot(data=slaveholders.60.rd,
       y.var="Personal Estate 1870",
       x.var="taxprop",
       ylab="1870 personal property value (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/persprop_70_slaveholders.png", plot=last_plot(), scale=1.25)

RdPlot(data=slaveholders.60.rd,
       y.var="taxprop.1870",
       x.var="taxprop",
       ylab="1870 taxable property value (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/taxprop_70_slaveholders.png", plot=last_plot(), scale=1.25)

RdPlot(data=slaveholders.60.rd,
       y.var="realprop.d",
       x.var="taxprop",
       ylab="Change in real estate value, 1860-1870 (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/realprop_d_slaveholders.png", plot=last_plot(), scale=1.25)

RdPlot(data=slaveholders.60.rd,
       y.var="persprop.d",
       x.var="taxprop",
       ylab="Change in personal property value, 1860-1870 (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/persprop_d_slaveholders.png", plot=last_plot(), scale=1.25)

RdPlot(data=slaveholders.60.rd,
       y.var="taxprop.d",
       x.var="taxprop",
       ylab="Change in taxable property value, 1860-1870 (1860$)",
       title="1860 slaveholders in the 1870 Census",
       continuous=TRUE) 
ggsave("data/plots/taxprop_d_slaveholders.png", plot=last_plot(), scale=1.25)