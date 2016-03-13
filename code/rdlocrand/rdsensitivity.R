
###################################################################

# rdsensitivity: window selection for randomization inference in RD

# !version 0.01 2-Feb-2016

# Authors: Matias Cattaneo, Rocio Titiunik, Gonzalo Vazquez-Bare

###################################################################

source('rdlocrand_fun.R')

rdsensitivity = function(Y,R,
                         cutoff = 0,
                         wlist,
                         tlist,
                         ci,
                         statistic = 'ttest',
                         nodraw = FALSE,
                         p = 0,
                         evalat = 'cutoff',
                         kernel = 'uniform',
                         reps = 1000,
                         seed = '',
                         fuzzy = '',
                         quietly = FALSE){



  #################################################################
  # Parameters and error checking
  #################################################################
  
  if (cutoff<=min(R,na.rm=TRUE) | cutoff>=max(R,na.rm=TRUE)){stop('Cutoff must be within the range of the running variable')}
  if (statistic!='ttest' & statistic!='ksmirnov' & statistic!='ranksum'){stop(paste(statistic,'not a valid statistic'))}
  if (evalat!='cutoff' & evalat!='means'){stop('evalat only admits means or cutoff')}
  
  Rc = R - cutoff
  
  
  #################################################################
  # Default window list
  #################################################################
  
  if (missing(wlist)){
    aux = rdwinselect(Rc,obsstep=5,quietly=TRUE)
    wlist = round(aux$results[,1],2)
  }
  
  
  #################################################################
  # Default tau list
  #################################################################
  
  if (missing(tlist)){
    D = as.numeric(Rc >= 0)
    aux = lm(Y ~ D)
    ci.ub = round(aux$coefficients['D']+1.96*sqrt(vcov(aux)['D','D']),2)
    ci.lb = round(aux$coefficients['D']-1.96*sqrt(vcov(aux)['D','D']),2)
    wstep = round((ci.ub-ci.lb)/10,2)
    tlist = seq(ci.lb,ci.ub,by=wstep)
  }
  
  
  #################################################################
  # Sensitivity analysis
  #################################################################
  
  results = array(NA,dim=c(length(tlist),length(wlist)))
  if (quietly==FALSE) {cat('\nRunning sensitivity analysis...\n')}
  row = 1
  for (t in tlist){
    col = 1
    for (w in wlist){
      if (evalat=='means'){
        ww = (Rc >= wl) & (Rc <= wr)
        Rw = R[ww]
        Dw = D[ww]
        evall = mean(Rw[Dw==0])
        evalr = mean(Rw[Dw==1])
      } else{
        evall=''
        evalr=''
      }
      
      aux = rdrandinf(Y,Rc,wl=-w,wr=w,p=p,reps=reps,nulltau=t,
                      statistic=statistic,kernel=kernel,evall=evall,evalr=evalr,
                      fuzzy=fuzzy,seed=seed,quietly=TRUE)
      results[row,col] = aux$p.value
      col = col + 1
    }
    row = row + 1
  }
  if (quietly==FALSE) {cat('\nSensitivity analysis complete.\n')}
  
  
  #################################################################
  # Confidence interval
  #################################################################
  
  if (!missing(ci)){
    ci.window = ci[1]
    if (length(ci)>1){
      ci.level = ci[2]
      if (ci.level<=0 | ci.level>=1){stop('ci level has to be between 0 and 1')}
    }else {
      ci.level = .05
    }
    if (is.element(ci.window,wlist)==TRUE){
      col = which(wlist==ci.window)
      aux = results[,col]
      if (all(aux>ci.level)){
        index.lb = 1
        index.ub = length(aux)
        ci.lb = tlist[index.lb]
        ci.ub = tlist[index.ub]
      } else if (all(aux<ci.level)){
        warning('no valid confidence interval in specified window grid')
        ci.lb = NA
        ci.ub = NA
      } else {
        index.lb = min(which(aux>=.05))
        index.ub = max(which(aux>=.05))
        ci.lb = tlist[index.lb]
        ci.ub = tlist[index.ub]
      }
      
      
      conf.int = c(ci.lb,ci.ub)
      
    } else{
      stop('window specified in ci not in wlist')
    }
  }


  #################################################################
  # Output
  #################################################################
  
  if (missing(ci)){
    output = list(tlist = tlist, wlist = wlist, results = results)
  } else {
    output = list(tlist = tlist, wlist = wlist, results = results, ci = conf.int)
  }
  
  
  #################################################################
  # Plot
  #################################################################
  
  if (nodraw==FALSE){
    if (dim(results)[2]==1){
      warning('need a window grid to draw plot')
    } else if (dim(results)[1]==1){
      warning('need a tau grid to draw plot')
    } else {
      filled.contour(wlist,tlist,t(results),
                     xlab='window',ylab='treatment effect',
                     key.title=title(main = 'p-value',cex.main=.8),
                     levels=seq(0,1,by=.01),col=gray.colors(100,1,0))
      
    }
  }
  
  return(output)

}


