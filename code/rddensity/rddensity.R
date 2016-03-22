################################################################################
# RDDENSITY R PACKAGE -- rddensity -- manipulation test
# Authors: Matias D. Cattaneo, Michael Jansson, Xinwei Ma
################################################################################
#!version 0.1  31Mar2015

source(paste0(code.directory,"rddensity/rdbwdensity.R"))

rddensity <- function(X, c=0, p=2, q=0, kernel="", fitselect="", hl=0, hr=0, hscale=1, bwselect="", vce="", print.screen=TRUE) {
  
  ################################################################################
  # default values 
  ################################################################################
  if (q==0) { q <- p+1 }
  if (kernel == "") { kernel <- "triangular" }
  kernel <- tolower(kernel)
  if (fitselect == "") { fitselect <- "unrestricted" }
  fitselect <- tolower(fitselect)
  if (bwselect == "") { bwselect <- "comb" }
  bwselect <- tolower(bwselect)
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
  if (p > q) { stop("p cannot exceed q.") }
  if (kernel!="uniform" & kernel!= "triangular" & kernel!="epanechnikov") { stop("kernel incorrectly specified.") }
  if (fitselect!="unrestricted" & fitselect!="restricted") { stop("fitselect incorrectly specified.") }
  if (hl<0 | hr<0) { stop("Bandwidth cannot be negative.") }
  if (fitselect=="restricted" & hl!=hr) { stop("Bandwidths must be equal in restricted model.") }
  if (hscale<=0 | hscale>1) { stop("hscale must be positive and no larger than 1.") }
  if (bwselect!="each" & bwselect!="diff" & bwselect!="sum" & bwselect!="comb") { stop("bwselect incorrectly specified.") }
  if (fitselect=="restricted" & bwselect=="each") { stop("bwselect=each is not available in the restricted model.") }
  if (vce!="plugin" & vce!="jackknife") { stop("vce incorrectly specified.") }
  # end of error handling
  
  ################################################################################
  # bandwidth selection
  ################################################################################
  if (hl > 0) { bwselectl <- "mannual" } else { bwselectl <- "estimated" }
  if (hr > 0) { bwselectr <- "mannual" } else { bwselectr <- "estimated" }
  if (hl==0 | hr==0) {
    out <- rdbwdensity(X=X, c=c, p=p, kernel=kernel, fitselect=fitselect, vce=vce, print.screen=FALSE)
    if (fitselect=="unrestricted" & bwselect=="each" & hl==0)  hl = out[1,1]
  	if (fitselect=="unrestricted" & bwselect=="each" & hr==0)  hr = out[2,1]
		if (fitselect=="unrestricted" & bwselect=="diff" & hl==0)  hl = out[3,1]
		if (fitselect=="unrestricted" & bwselect=="diff" & hr==0)  hr = out[3,1]
		if (fitselect=="unrestricted" & bwselect=="sum"  & hl==0)  hl = out[4,1]
		if (fitselect=="unrestricted" & bwselect=="sum"  & hr==0)  hr = out[4,1]
		if (fitselect=="unrestricted" & bwselect=="comb" & hl==0)  hl = median(c(out[1,1], out[3,1], out[4,1]))
		if (fitselect=="unrestricted" & bwselect=="comb" & hr==0)  hr = median(c(out[2,1], out[3,1], out[4,1]))

		if (fitselect=="restricted" & bwselect=="diff" & hl==0)  hl = out[3,1]
		if (fitselect=="restricted" & bwselect=="diff" & hr==0)  hr = out[3,1]
		if (fitselect=="restricted" & bwselect=="sum"  & hl==0)  hl = out[4,1]
		if (fitselect=="restricted" & bwselect=="sum"  & hr==0)  hr = out[4,1]
		if (fitselect=="restricted" & bwselect=="comb" & hl==0)  hl = min(c(out[3,1], out[4,1]))
		if (fitselect=="restricted" & bwselect=="comb" & hr==0)  hr = min(c(out[3,1], out[4,1]))
  }
  # end of bandwidth selection
  
  ################################################################################
  # data trimming
  ################################################################################
  X <- X - c; Y <- (0:(N-1)) / (N-1); 
  Xh <- X[(X>=-1*hl*hscale) & (X<=hr*hscale)]; Yh <- Y[(X>=-1*hl*hscale) & (X<=hr*hscale)]
  Nlh <- sum(Xh < 0); Nrh <- sum(Xh >= 0)
  if (Nlh < 5 | Nrh < 5) { stop("Not enough observations to perform calculation.") }
  Nh <- Nlh + Nrh
  # end of data trimming
  
  ################################################################################
  # estimation
  ################################################################################
  fV_q <- rddensity_fV(Y=Yh, X=Xh, Nl=Nl, Nr=Nr, Nlh=Nlh, Nrh=Nrh, hl=hl*hscale, hr=hr*hscale, p=q, s=1, kernel=kernel, fitselect=fitselect)
  T_asy <- fV_q[3, 1] / sqrt(fV_q[3, 3]); T_jk <- fV_q[3, 1] / sqrt(fV_q[3, 2])
  p_asy <- pnorm(abs(T_asy), lower.tail=FALSE) * 2; p_jk <- pnorm(abs(T_jk), lower.tail=FALSE) * 2
  
  result <- list( hat=   list(left=fV_q[1,1], right=fV_q[2,1], diff=fV_q[3,1]), 
                  sd.asy=list(left=sqrt(fV_q[1,3]), right=sqrt(fV_q[2,3]), diff=sqrt(fV_q[3,3])), 
                  sd.jk= list(left=sqrt(fV_q[1,2]), right=sqrt(fV_q[2,2]), diff=sqrt(fV_q[3,2])),
                  test=  list(t.asy=T_asy, t.jk=T_jk, p.asy=p_asy, p.jk=p_jk), 
                  N=     list(full=N, left=Nl, right=Nr, eff.left=Nlh, eff.right=Nrh), 
                  h=     list(left=hl*hscale, right=hr*hscale))
  
  if (print.screen) {
    cat("\nRD Manipulation Test using local polynomial density estimation.\n")
    cat("\n")
    
    cat(paste(format("Number of obs =", width=30), toString(N), sep="")); cat("\n")
    cat(paste(format("Model =", width=30), fitselect, sep="")); cat("\n")
    cat(paste(format("Kernel =", width=30), kernel, sep="")); cat("\n")
    if (bwselectl!="mannual" | bwselectr!="mannual") {
      cat(paste(format("BW method =", width=30), bwselect, sep="")); cat("\n")
    }
    if (hscale < 1) {
      cat(paste(format("BW scale =", width=30), toString(round(hscale, 3)), sep="")); cat("\n")
    }
    cat(paste(format("VCE method =", width=30), vce, sep="")); cat("\n")
    cat("\n")
    
    cat(paste(format(paste("Cutoff c = ", toString(round(c, 3)), sep=""), width=30), format("Left of c", width=20), format("Right of c", width=20), sep="")); cat("\n")
    cat(paste(format("Number of obs", width=30), format(toString(Nl), width=20), format(toString(Nr), width=20), sep="")); cat("\n")
    cat(paste(format("Eff. Number of obs", width=30), format(toString(Nlh), width=20), format(toString(Nrh), width=20), sep="")); cat("\n")
    cat(paste(format("Min Running var.", width=30), format(toString(round(min(X[X<0]+c), 3)), width=20), format(toString(round(min(X[X>=0]+c), 3)), width=20), sep="")); cat("\n")
    cat(paste(format("Max Running var.", width=30), format(toString(round(max(X[X<0]+c), 3)), width=20), format(toString(round(max(X[X>=0]+c), 3)), width=20), sep="")); cat("\n")
    cat(paste(format("Order loc. poly. (p)", width=30), format(toString(p), width=20), format(toString(p), width=20), sep="")); cat("\n")
    if (q > p) {
      cat(paste(format("Order BC (q)", width=30), format(toString(q), width=20), format(toString(q), width=20), sep="")); cat("\n")
    } else {
      cat(paste(format("No bias correction (q)", width=30), format(toString(q), width=20), format(toString(q), width=20), sep="")); cat("\n")
    }
    
    cat(paste(format("Bandwidths (hl,hr)", width=30), format(bwselectl, width=20), format(bwselectr, width=20), sep="")); cat("\n")
    if (hscale < 1) {
      cat(paste(format("Bandwidth values", width=30), format(paste(toString(round(hl, 3)), " * ", toString(round(hscale, 3)), sep=""), width=20), format(paste(toString(round(hr, 3)), " * ", toString(round(hscale, 3)), sep=""), width=20), sep="")); cat("\n")
    } else {
      cat(paste(format("Bandwidth values", width=30), format(toString(round(hl, 3)), width=20), format(toString(round(hr, 3)), width=20), sep="")); cat("\n")
    }
    cat("\n")
    
    cat(paste(format("Method", width=30), format("T", width=20), format("P > |T|", width=20), sep="")); cat("\n")
    if (p == q & hscale==1) {
      if (vce == "plugin") {
        cat(paste(format("Conventional", width=30), format(toString(round(T_asy, 4)), width=20), format(toString(round(p_asy, 4)), width=20), sep="")); cat("\n")
      } else {
        cat(paste(format("Conventional", width=30), format(toString(round(T_jk, 4)), width=20), format(toString(round(p_jk, 4)), width=20), sep="")); cat("\n")
      }      
    } else if (p == q & hscale<1) {
      if (vce == "plugin") {
        cat(paste(format("Undersmoothed", width=30), format(toString(round(T_asy, 4)), width=20), format(toString(round(p_asy, 4)), width=20), sep="")); cat("\n")
      } else {
        cat(paste(format("Undersmoothed", width=30), format(toString(round(T_jk, 4)), width=20), format(toString(round(p_jk, 4)), width=20), sep="")); cat("\n")
      }  
    } else if (p < q & hscale == 1) {
      if (vce == "plugin") {
        cat(paste(format("Robust Bias-Corrected", width=30), format(toString(round(T_asy, 4)), width=20), format(toString(round(p_asy, 4)), width=20), sep="")); cat("\n")
      } else {
        cat(paste(format("Robust Bias-Corrected", width=30), format(toString(round(T_jk, 4)), width=20), format(toString(round(p_jk, 4)), width=20), sep="")); cat("\n")
      }  
    } else {
      if (vce == "plugin") {
        cat(paste(format("Robust BC, Undersmoothed", width=30), format(toString(round(T_asy, 4)), width=20), format(toString(round(p_asy, 4)), width=20), sep="")); cat("\n")
      } else {
        cat(paste(format("Robust BC, Undersmoothed", width=30), format(toString(round(T_jk, 4)), width=20), format(toString(round(p_jk, 4)), width=20), sep="")); cat("\n")
      }  
    }
    cat("\n")
    if (p==q & hscale==1) {
      warning("Without bias correction or undersmoothing, the size of the test may not be correct.") 
    }
    if (p==1 & q==1) {
      warning("With p=q=1, the size of the test may not be correct.")
    }
    cat("\n")
    
  } else {
    return(result)
  }
}







