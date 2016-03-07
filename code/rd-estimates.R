# RD estimates

require(ggplot2)
require(reporttools)
require(sandwich)
require(lmtest)
require(rdrobust)
require(scales)
require(reshape2)

#source("delegates.R") # Run delegates
#source("rd-balance.R") # Create balance plots 

# Estimates for summary figure
local.lin <- lm(dv ~ treat + rv + rv_treat, data=hall[(hall$margin < .05) & (hall$absdist > cutoff),]) # column 1
local.lin.r <- coeftest(local.lin, vcov = vcovHC(local.lin, "HC1"))  # robust; HC1 (Stata default)

cubic <- lm(dv ~ treat + rv2 + rv3, data=hall[hall$absdist > cutoff,]) # column 2
cubic.r <- coeftest(cubic, vcov = vcovHC(cubic, "HC1"))  # robust; HC1 (Stata default)

ik <- rdrobust(hall$dv, # column 3. Default CI is 95.
               hall$rv,
               bwselect="IK")  

local.lin2 <- lm(dv_win ~ treat + rv + rv_treat, data=hall[(hall$margin < .05) & (hall$absdist > cutoff),]) # column 4
local.lin.r2 <- coeftest(local.lin2, vcov = vcovHC(local.lin, "HC1"))  # robust; HC1 (Stata default)

cubic2 <- lm(dv_win ~ treat + rv2 + rv3, data=hall[hall$absdist > cutoff,]) # column 5
cubic.r2 <- coeftest(cubic2, vcov = vcovHC(cubic, "HC1"))  # robust; HC1 (Stata default)

ik2 <- rdrobust(hall$dv_win, # column 6
                hall$rv,
                bwselect="IK")  

cct <- rdrobust(hall$dv,
                hall$rv)  

cct2 <- rdrobust(hall$dv_win,
                 hall$rv)  

# Create data for plot
tab2.dat <- data.frame(x = c("Vote share general election","Victory general election"),
                       y = c(local.lin.r[2], local.lin.r2[2],
                             cubic.r[2],cubic.r2[2],
                             ik$coef[1],ik2$coef[1],
                             cct$coef[3],cct2$coef[3]),
                       y.lo = c(local.lin.r[2]-local.lin.r[6], 
                                local.lin.r2[2]-local.lin.r2[6],
                                cubic.r[2]-cubic.r[6],
                                cubic.r2[2]-cubic.r2[6],
                                ik$ci[1],
                                ik2$ci[1],
                                cct$ci[3],
                                cct2$ci[3]),
                       y.hi = c(local.lin.r[2]+local.lin.r[6], 
                                local.lin.r2[2]+local.lin.r2[6],
                                cubic.r[2]+cubic.r[6],
                                cubic.r2[2]+cubic.r2[6],
                                ik$ci[4],
                                ik2$ci[4],
                                cct$ci[6],
                                cct2$ci[6]))
tab2.dat$Specification <- c(rep("Local linear (robust)",2),
                            rep("Cubic (robust)",2),
                            rep("IK (conventional)",2),
                            rep("CCT (robust)",2))
# Plot forest plot
suppressWarnings(tab2.dat$x <- factor(tab2.dat$x, levels=rev(tab2.dat$x))) # reverse order
ForestPlot(tab2.dat,xlab="RD Estimate on Victory",ylab="") + facet_grid(Specification ~ .)