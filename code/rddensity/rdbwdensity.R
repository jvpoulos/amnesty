################################################################################
# RDDENSITY R PACKAGE -- rddensity -- bandwidth choice
# Authors: Matias D. Cattaneo, Michael Jansson, Xinwei Ma
################################################################################
#!version 0.1  31Mar2015

source(paste0(code.directory,"rddensity/rddensity_fun.R"))

rdbwdensity <- function(X, c=0, p=2, kernel="", fitselect="", vce="", print.screen=TRUE) {
  
  ################################################################################
  # default values 
  ################################################################################
  if (kernel == "") { kernel <- "triangular" }
  kernel <- tolower(kernel)
  if (fitselect == "") { fitselect <- "unrestricted" }
  fitselect <- tolower(fitselect)
  if (vce == "") { vce <- "jackknife" }
  vce <- tolower(vce)
  # end of default values
  
  ################################################################################
  # sample sizes
  ################################################################################
  X <- sort(X)
  N <- length(X); Nl <- sum(X<c); Nr <- sum(X>=c); Xmin <- min(X); Xmax <- max(X)
  # end of sample sizes
  
  ################################################################################
  # error handling
  ################################################################################
  if (c <= Xmin | c >= Xmax) { stop("The cutoff should be set within the range of the data.") }
  if (Nl <= 10 | Nr <= 10) { stop("Not enough observations to perform calculations.") }
  if (p!=1 & p!=2 & p!=3 & p!=4 & p!=5 & p!=6 & p!= 7) { stop("p must be an integer between 1 and 7.") }
  if (kernel!="uniform" & kernel!= "triangular" & kernel!="epanechnikov") { stop("kernel incorrectly specified.") }
  if (fitselect!="unrestricted" & fitselect!="restricted") { stop("fitselect incorrectly specified.") }
  if (vce!="plugin" & vce!="jackknife") { stop("vce incorrectly specified.") }
  # end of error handling
  
  ################################################################################
  # select preliminary bandwidth
  ################################################################################
  X <- X - c; Xmu <- mean(X); Xsd <- sd(X)
  fhatb <- 1 / (rddensity_H(Xmu / Xsd, p+2)^2 * dnorm(Xmu / Xsd))
  fhatc <- 1 / (rddensity_H(Xmu / Xsd, p)^2 * dnorm(Xmu / Xsd))
  # these constants are for uniform kernel
  Cb <- c(25884.444444494150957,3430865.4551236177795,845007948.04262602329,330631733667.03808594,187774809656037.3125,145729502641999264,146013502974449876992)
  Cc <- c(4.8000000000000246914,548.57142857155463389,100800.00000020420703,29558225.458100609481,12896196859.612621307,7890871468221.609375,6467911284037581)
  bn <- ((2*p+1)/4 * fhatb * Cb[p] / N)^(1/(2*p+5))
  cn <- (1/(2*p) * fhatc * Cc[p] / N)^(1/(2*p+1))
  bn <- bn * Xsd; cn <- cn * Xsd
  # end of select preliminary bandwidth
  
  ################################################################################
  # estimate main bandwidth
  ################################################################################
  Y <- (0:(N-1)) / (N-1)
  Yb <- Y[abs(X) <= bn]; Xb <- X[abs(X) <= bn]; Yc <- Y[abs(X) <= cn]; Xc <- X[abs(X) <= cn]
  Nlb <- sum(Xb < 0); Nrb <- sum(Xb >= 0); Nlc <- sum(Xc < 0); Nrc <- sum(Xc >= 0)
  
  hn <- matrix(NA, ncol=3, nrow=4)
  colnames(hn) <- c("bw", "variance", "biassq"); rownames(hn) <- c("l", "r", "diff", "sum")
  fV_b <- rddensity_fV(Y=Yb, X=Xb, Nl=Nl, Nr=Nr, Nlh=Nlb, Nrh=Nrb, hl=bn, hr=bn, p=p+2, s=p+1, kernel=kernel, fitselect=fitselect)
  fV_c <- rddensity_fV(Y=Yc, X=Xc, Nl=Nl, Nr=Nr, Nlh=Nlc, Nrh=Nrc, hl=cn, hr=cn, p=p, s=1, kernel=kernel, fitselect=fitselect)
  
  if (vce == "plugin") { hn[, 2] <- N * cn * fV_c[, 3]  } else { hn[, 2] <- N * cn * fV_c[, 2] }
  if (fitselect == "unrestricted") {
    S <- S.generate(p=p, low=0, up=1, kernel=kernel); C <- C.generate(k=p+1, p=p, low=0, up=1, kernel=kernel)
    hn[1, 3] <- fV_b[1, 4] * (solve(S) %*% C)[2] * (-1)^p
    hn[2, 3] <- fV_b[2, 4] * (solve(S) %*% C)[2]
    hn[3, 3] <- hn[2, 3] - hn[1, 3]; hn[4, 3] <- hn[2, 3] + hn[1, 3]
  } else {
    Splus <- S.plus.generate(p=p, kernel=kernel); Cplus <- C.plus.generate(k=p+1, p=p, kernel=kernel); Psi <- Psi.generate(p=p)
    Sinv <- solve(fV_c[2, 1] * Splus + fV_c[1, 1] * Psi%*%Splus%*%Psi)
    C <- fV_b[1, 4] * (fV_c[2, 1] * Cplus + (-1)^(p+1) * fV_c[1, 1] * Psi%*%Cplus)
    temp <- Sinv%*%C
    hn[1, 3] <- temp[2]; hn[2, 3] <- temp[3]; hn[3, 3] <- hn[2, 3] - hn[1, 3]; hn[4, 3] <- hn[2, 3] + hn[1, 3]
  }
  
  hn[, 3] <- hn[, 3]^2
  hn[, 1] <- (1/(2*p) * hn[, 2] / hn[, 3] / N)^(1/(2*p+1))
  # end of estimate main bandwidth
  
  for (i in 1:4) {
    if (hn[i, 2] < 0) { hn[i, 1] <- 0; hn[i, 2] <- NA }
    if (is.na(hn[i, 1])) { hn[i, 1] <- 0 }
  }
  
  ################################################################################
  # output the results
  ################################################################################
  if (print.screen) {
    cat("\nBandwidth selection for manipulation testing.\n")
    cat("\n")
    
    cat(paste(format("Number of obs =", width=30), toString(N), sep="")); cat("\n")
    cat(paste(format("Model =", width=30), fitselect, sep="")); cat("\n")
    cat(paste(format("Kernel =", width=30), kernel, sep="")); cat("\n")
    cat(paste(format("VCE method =", width=30), vce, sep="")); cat("\n")
    cat("\n")
    
    cat(paste(format(paste("Cutoff c = ", toString(round(c, 3)), sep=""), width=30), format("Left of c", width=20), format("Right of c", width=20), sep="")); cat("\n")
    cat(paste(format("Number of obs", width=30), format(toString(Nl), width=20), format(toString(Nr), width=20), sep="")); cat("\n")
    cat(paste(format("Min Running var.", width=30), format(toString(round(min(X[X<0]+c), 3)), width=20), format(toString(round(min(X[X>=0]+c), 3)), width=20), sep="")); cat("\n")
    cat(paste(format("Max Running var.", width=30), format(toString(round(max(X[X<0]+c), 3)), width=20), format(toString(round(max(X[X>=0]+c), 3)), width=20), sep="")); cat("\n")
    cat(paste(format("Order loc. poly. (p)", width=30), format(toString(p), width=20), format(toString(p), width=20), sep="")); cat("\n")
    cat("\n")
    
    cat(paste(format("Target", width=30), format("Bandwidth", width=20), format("Variance", width=20), format("Bias^2", width=20), sep="")); cat("\n")
    cat(paste(format("left density", width=30), format(toString(round(hn[1,1], 4)), width=20), format(toString(round(hn[1,2], 4)), width=20), format(toString(round(hn[1,3], 4)), width=20), sep="")); cat("\n")
    cat(paste(format("right density", width=30), format(toString(round(hn[2,1], 4)), width=20), format(toString(round(hn[2,2], 4)), width=20), format(toString(round(hn[2,3], 4)), width=20), sep="")); cat("\n")
    cat(paste(format("difference densities", width=30), format(toString(round(hn[3,1], 4)), width=20), format(toString(round(hn[3,2], 4)), width=20), format(toString(round(hn[3,3], 4)), width=20), sep="")); cat("\n")
    cat(paste(format("sum densities", width=30), format(toString(round(hn[4,1], 4)), width=20), format(toString(round(hn[4,2], 4)), width=20), format(toString(round(hn[4,3], 4)), width=20), sep="")); cat("\n")
    cat("\n")
  } else {
  return(data.frame(hn))
  }
}









