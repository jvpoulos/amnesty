
###################################################################

# rdrbounds: Rosenbaum bounds for randomization inference in RD

# !version 0.01 2-Feb-2016

# Authors: Matias Cattaneo, Rocio Titiunik, Gonzalo Vazquez-Bare

###################################################################

# source('rdlocrand_fun.R')

rdrbounds = function(Y,R,
                     cutoff = 0,
                     prob,
                     reps = 1000,
                     bound = 'both',
                     wlist,
                     gamma,
                     expgamma,
                     fmpval = FALSE,
                     statistic = 'ranksum',
                     p = 0,
                     evalat = 'cutoff',
                     kernel = 'uniform',
                     nulltau = 0,
                     seed = '',
                     fuzzy = ''){
  
  
  #################################################################
  # Parameters and error checking
  #################################################################
  
  if (cutoff<=min(R,na.rm=TRUE) | cutoff>=max(R,na.rm=TRUE)){stop('Cutoff must be within the range of the running variable')}
  if (bound!='both' & bound!='upper' & bound!='lower'){stop('bound option incorrectly specified')}
  
  Rc = R - cutoff
  D = Rc >= 0
  
  if (missing(gamma) & missing(expgamma)){
    gamma = c(.1,.5,.9)
    gammalist = exp(gamma)
  }
  if (!missing(gamma) & missing(expgamma)){
    gammalist = exp(gamma)
  }
  if (missing(gamma) & !missing(expgamma)){
    gammalist = expgamma
  }
  if (!missing(gamma) & !missing(expgamma)){
    stop('gamma and expgamma cannot be specified simultaneously')
  }
  
  if (missing(wlist)){
    aux = rdwinselect(Rc,obsstep=5,nwindows=5,quietly=TRUE)
    wlist = round(aux$results[,1],2)
  }
  
  evall = cutoff
  evalr = cutoff
  
  
  #################################################################
  # Randomization p-value and sensitivity analysis
  #################################################################
  
  cat('\nCalculating randomization p-value...\n')
  P = array(NA,dim=c(2,length(wlist)))
  
  count = 1
  
  if (fmpval==FALSE){
    for (w in wlist){
      ww = (Rc >= -w) & (Rc <= w)
      Dw = D[ww]
      Rw = Rc[ww]
      if (missing(prob)){
        prob = rep(mean(Dw),length(R))
      }
      if (evalat=='means'){
        evall = mean(Rw[Dw==0])
        evalr = mean(Rw[Dw==1])
      }
      aux = rdrandinf(Y,R,wl=-w,wr=w,bernoulli=prob,reps=reps,p=p,
                      nulltau=nulltau,statistic=statistic,
                      evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                      quietly=TRUE)
      P[1,count] = aux$p.value
      
      cat(paste0('\nBernoulli p-value (w = ',w,') = ',round(P[1,count],3)))
      
      count = count + 1
    }
  } else {
    for (w in wlist){
      ww = (Rc >= -w) & (Rc <= w)
      Dw = D[ww]
      Rw = Rc[ww]
      if (missing(prob)){
        prob = rep(mean(Dw),length(R))
      }
      if (evalat=='means'){
        evall = mean(Rw[Dw==0])
        evalr = mean(Rw[Dw==1])
      }
      
      aux.be = rdrandinf(Y,R,wl=-w,wr=w,bernoulli=prob,reps=reps,p=p,
                         nulltau=nulltau,statistic=statistic,
                         evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                         quietly=TRUE)
      P[1,count] = aux.be$p.value
      
      aux.fm = rdrandinf(Y,R,wl=-w,wr=w,reps=reps,p=p,
                         nulltau=nulltau,statistic=statistic,
                         evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                         quietly=TRUE)
      
      P[2,count] = aux.fm$p.value
      
      cat(paste0('\nBernoulli p-value (w = ',w,') = ',round(P[1,count],3)))
      cat(paste0('\nFixed margins p-value (w = ',w,') = ',round(P[2,count],3)))
      
      count = count + 1
      
    }
  }
  
  cat('\n')

  #################################################################
  # Sensitivity analysis
  #################################################################
  

  cat('\nRunning sensitivity analysis...\n')
  
  count.g = 1
  
  if (bound=='upper'){
    
    p.ub = array(NA,dim=c(length(gammalist),length(wlist)))
    
    for (G in gammalist){
      
      plow = 1/(1+G)
      phigh = G/(1+G)
      
      count.w = 1
      
      for (w in wlist){
        
        ww = (Rc >= -w) & (Rc <= w)
        Dw = D[ww]
        Yw = Y[ww]
        Rw = R[ww]
        
        data = cbind(Yw,Rw,Dw)
        data = data[complete.cases(data),]
        Yw = data[,1]
        Rw = data[,2]
        Dw = data[,3]
        
        jj = order(data[,1],decreasing=TRUE)
        data.dec = data[jj,]
        Yw.dec = data.dec[,1]
        Rw.dec = data.dec[,2]
        
        nw = length(Rw)
        nw1 = sum(Dw)
        nw0 = nw - nw1
        pvals.ub = NULL

        for (u in seq(1,nw)){
          
          uplus = c(rep(1,u),rep(0,nw-u))
          p.aux = phigh*uplus + plow*(1-uplus)
          aux = rdrandinf(Yw.dec,Rw.dec,wl=-w,wr=w,bernoulli=p.aux,reps=reps,p=p,
                          nulltau=nulltau,statistic=statistic,
                          evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                          quietly=TRUE)
          pvals.ub = c(pvals.ub,aux$p.value)

        }
        
        p.ub.w = max(pvals.ub)
        p.ub[count.g,count.w] = p.ub.w

        count.w = count.w + 1
        
      }
      
      count.g = count.g + 1
      
    }
  }
  
  if (bound=='both'){
    
    p.ub = array(NA,dim=c(length(gammalist),length(wlist)))
    p.lb = array(NA,dim=c(length(gammalist),length(wlist)))
    
    for (G in gammalist){
      
      plow = 1/(1+G)
      phigh = G/(1+G)
      
      count.w = 1
      
      for (w in wlist){
        
        ww = (Rc >= -w) & (Rc <= w)
        Dw = D[ww]
        Yw = Y[ww]
        Rw = R[ww]
        
        data = cbind(Yw,Rw,Dw)
        data = data[complete.cases(data),]
        Yw = data[,1]
        Rw = data[,2]
        Dw = data[,3]
        
        ii = order(data[,1])
        data.inc = data[ii,]
        Yw.inc = data.inc[,1]
        Rw.inc = data.inc[,2]
        jj = order(data[,1],decreasing=TRUE)
        data.dec = data[jj,]
        Yw.dec = data.dec[,1]
        Rw.dec = data.dec[,2]
        
        nw = length(Rw)
        nw1 = sum(Dw)
        nw0 = nw - nw1
        pvals.ub = NULL
        pvals.lb = NULL

        for (u in seq(1,nw)){
          
          uplus = c(rep(1,u),rep(0,nw-u))
          p.aux = phigh*uplus + plow*(1-uplus)
          aux = rdrandinf(Yw.dec,Rw.dec,wl=-w,wr=w,bernoulli=p.aux,reps=reps,p=p,
                          nulltau=nulltau,statistic=statistic,
                          evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                          quietly=TRUE)
          pvals.ub = c(pvals.ub,aux$p.value)
          uminus = c(rep(0,nw-u),rep(1,u))
          p.aux = phigh*uminus + plow*(1-uminus)
          aux = rdrandinf(Yw.inc,Rw.inc,wl=-w,wr=w,bernoulli=p.aux,reps=reps,p=p,
                          nulltau=nulltau,statistic=statistic,
                          evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                          quietly=TRUE)        
          pvals.lb = c(pvals.lb,aux$p.value)
          
        }
        
        p.ub.w = max(pvals.ub)
        p.lb.w = min(pvals.lb)
        p.ub[count.g,count.w] = p.ub.w
        p.lb[count.g,count.w] = p.lb.w
        
        count.w = count.w + 1
        
      }
      
      count.g = count.g + 1
      
    }
  }
  
  if (bound=='lower'){

    p.lb = array(NA,dim=c(length(gammalist),length(wlist)))
    
    for (G in gammalist){
      
      plow = 1/(1+G)
      phigh = G/(1+G)
      
      count.w = 1
      
      for (w in wlist){
        
        ww = (Rc >= -w) & (Rc <= w)
        Dw = D[ww]
        Yw = Y[ww]
        Rw = R[ww]
        
        data = cbind(Yw,Rw,Dw)
        data = data[complete.cases(data),]
        Yw = data[,1]
        Rw = data[,2]
        Dw = data[,3]
        
        ii = order(data[,1])
        data.inc = data[ii,]
        Yw.inc = data.inc[,1]
        Rw.inc = data.inc[,2]

        nw = length(Rw)
        nw1 = sum(Dw)
        nw0 = nw - nw1
        pvals.ub = NULL
        pvals.lb = NULL
        
        for (u in seq(1,nw)){
          
          uminus = c(rep(0,nw-u),rep(1,u))
          p.aux = phigh*uminus + plow*(1-uminus)
          aux = rdrandinf(Yw.inc,Rw.inc,wl=-w,wr=w,bernoulli=p.aux,reps=reps,p=p,
                          nulltau=nulltau,statistic=statistic,
                          evall=evall,evalr=evalr,kernel=kernel,fuzzy=fuzzy,
                          quietly=TRUE)        
          pvals.lb = c(pvals.lb,aux$p.value)
          
        }
        
        p.lb.w = min(pvals.lb)
        p.lb[count.g,count.w] = p.lb.w
        
        count.w = count.w + 1
        
      }
      
      count.g = count.g + 1
      
    }
  }
  
  cat('\nSensitivity analysis complete.\n')
  
  
  #################################################################
  # Output
  #################################################################
  
  if (fmpval==FALSE){
    P = P[-2,]
  }
  
  output = list(gamma = log(gammalist), expgamma = gammalist, wlist = wlist,
                p.values = P, lower.bound = p.lb, upper.bound = p.ub)
  
  return(output)
  
}