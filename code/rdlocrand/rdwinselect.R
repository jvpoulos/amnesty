
###################################################################

# rdwinselect: window selection for randomization inference in RD

# !version 0.01 2-Feb-2016

# Authors: Matias Cattaneo, Rocio Titiunik, Gonzalo Vazquez-Bare

###################################################################

source('rdlocrand_fun.R')

rdwinselect = function(R, X, 
                       cutoff=0, 
                       obsmin='', 
                       obsstep = '', 
                       wmin = '', 
                       wstep = '', 
                       nwindows = 10,
                       statistic = 'ttest', 
                       approx = FALSE, 
                       p = 0, 
                       evalat = 'cutoff', 
                       kernel = 'uniform', 
                       reps = 1000, 
                       seed = '', 
                       level = .15, 
                       plot = FALSE,
                       quietly = FALSE) {


  #################################################################
  # Parameters and error checking
  #################################################################
  
  if (cutoff<=min(R,na.rm=TRUE) | cutoff>=max(R,na.rm=TRUE)){stop('Cutoff must be within the range of the running variable')}
  if (p<0){stop('p must be a positive integer')}
  if (p>0 & approx==TRUE & statistic!='ttest'){stop('approximate and p>1 can only be combined with ttest')}
  if (statistic!='ttest' & statistic!='ksmirnov' & statistic!='ranksum' & statistic!='hotelling'){stop(paste(statistic,'not a valid statistic'))}
  if (evalat!='cutoff' & evalat!='means'){stop('evalat only admits means or cutoff')}
  if (kernel!='uniform' & kernel!='triangular' & kernel!='epan'){stop(paste(kernel,'not a valid kernel'))}
  if (kernel!='uniform' & evalat!='cutoff'){stop('kernel can only be combined with evalat(cutoff)')}
  if (kernel!='uniform' & statistic!='ttest'){stop('kernel only allowed for ttest')}
  
  Rc = R - cutoff
  D = R >= 0
  n = length(D[!is.na(D)])
  n1 = sum(D,na.rm=TRUE)
  n0 = n - n1
  
  if (!missing(X)){X = as.matrix(X)}
  if (seed!=''){set.seed(seed)}
  
  if (approx==FALSE){testing.method='rdrandinf'}else{testing.method='approximate'}
  
  ## Display upper-right panel
  
  if (quietly==FALSE){
    cat('\n')
    cat('\nWindow selection for RD under local randomization \n')
    cat('\n')
    cat(paste0(format('Number of obs  =', width=25), toString(n))); cat("\n")
    cat(paste0(format('Order of poly  =', width=25), p)); cat("\n")
    cat(paste0(format('Kernel type    =', width=25), kernel)); cat("\n")
    cat(paste0(format('Reps           =', width=25), reps)); cat("\n")
    cat(paste0(format('Testing method =', width=25), testing.method)); cat("\n")
    cat(paste0(format('Balance test   =', width=25), statistic))
    cat('\n\n')
  }
  
  
  #################################################################
  # Initial window, steps and window list
  #################################################################
  
  ## Define smallest window 
  
  if (wmin!=''){
    if (obsmin!=''){
      stop('Cannot specify both obsmin and wmin')
    } else{
      obsmin = 10
    }
  } else{
    if (obsmin==''){
      obsmin = 10
    }
    wmin = wlength(Rc,D,obsmin)
  }
  
  ## Define step
  
  if (wstep==''){
    if (obsstep==''){
      obsstep = 2
    }
    wstep = findstep(Rc,D,obsmin,obsstep,10)
  } else{
    if (obsstep!=''){
      stop('Cannot specify both obsstep and wstep')
    }
  }
  
  window.list = seq(from=wmin,by=wstep,length.out=nwindows)
  
  
  #################################################################
  # Summary statistics
  #################################################################
  
  table.sumstats = array(NA,dim=c(5,2))
  table.sumstats[1,] = c(n0,n1)
  
  qq0 = round(quantile(abs(Rc[D==0]),probs = c(.01,.05,.1,.2),type=1,na.rm=TRUE),5)
  qq1 = round(quantile(Rc[D==1],probs = c(.01,.05,.1,.2),type=1,na.rm=TRUE),5)
  
  n0.q1 = sum(Rc>=-qq0[1]& Rc<0,na.rm=TRUE)
  n0.q2 = sum(Rc>=-qq0[2]& Rc<0,na.rm=TRUE)
  n0.q3 = sum(Rc>=-qq0[3]& Rc<0,na.rm=TRUE)
  n0.q4 = sum(Rc>=-qq0[4]& Rc<0,na.rm=TRUE)
  n1.q1 = sum(Rc<=qq1[1]& Rc>=0,na.rm=TRUE)
  n1.q2 = sum(Rc<=qq1[2]& Rc>=0,na.rm=TRUE)
  n1.q3 = sum(Rc<=qq1[3]& Rc>=0,na.rm=TRUE)
  n1.q4 = sum(Rc<=qq1[4]& Rc>=0,na.rm=TRUE)
  
  table.sumstats[2,] = c(n0.q1,n1.q1)
  table.sumstats[3,] = c(n0.q2,n1.q2)
  table.sumstats[4,] = c(n0.q3,n1.q3)
  table.sumstats[5,] = c(n0.q4,n1.q4)
  
  ## Display upper left panel
  
  if (quietly==FALSE){
    cat(paste0(format(paste0("Cutoff c = ", toString(round(cutoff, 3))), width=25), format("Left of c", width=20), format("Right of c", width=20))); cat("\n")
    cat(paste0(format("Number of obs",   width=25), format(toString(n0), width=20), format(toString(n1), width=20))); cat("\n")
    cat(paste0(format("1st percentile",  width=25), format(toString(n0.q1), width=20), format(toString(n1.q1), width=20))); cat("\n")
    cat(paste0(format("5th percentile",  width=25), format(toString(n0.q2), width=20), format(toString(n1.q2), width=20))); cat("\n")
    cat(paste0(format("10th percentile", width=25), format(toString(n0.q3), width=20), format(toString(n1.q3), width=20))); cat("\n")
    cat(paste0(format("20th percentile", width=25), format(toString(n0.q4), width=20), format(toString(n1.q4), width=20)))
    cat("\n\n")
  }
  
  
  #################################################################
  # Balance tests
  #################################################################
  
  table.rdw = array(NA,dim=c(length(window.list),5))
  col = 1
  
  ## Being main panel display
  
  if (quietly==FALSE){
    cat(paste(format('Window length / 2', width=23), 
              format('p-value ',          width=12), 
              format('Var. name',         width=15),
              format('Bin.test',          width=12),
              format('Obs<c',             width=6),
              format('Obs>=c',            width=5)))
    cat('\n\n')
  }
      
  for (w in window.list){
    
    ww = (Rc >= -w) & (Rc <= w)
    
    Dw = D[ww]
    Rw = Rc[ww]
    
    ## Drop NA values
    
    if (!missing(X)){
      Xw = X[ww,]
      data = cbind(Rw,Dw,Xw)
      data = data[complete.cases(data),]
      Rw = data[,1]
      Dw = data[,2]
      Xw = data[,c(-1,-2)]
    } else {
      data = cbind(Rw,Dw)
      data = data[complete.cases(data),]
      Rw = data[,1]
      Dw = data[,2]
    }

    ## Sample sizes
    
    n0.w = length(Dw) - sum(Dw)
    n1.w = sum(Dw)
    n.w = n0.w+n1.w
    table.rdw[col,4] = n0.w
    table.rdw[col,5] = n1.w
    
    ## Binomial test
    
    bitest = binom.test(sum(Dw),length(Dw),p=0.5)
    table.rdw[col,3] = bitest$p.value
    
    if (!missing(X)){
      
      ## Weights
      
      kweights = rep(1,n.w)
      
      if (kernel=='triangular'){
        kweights = (1-abs(Rw/w))*(abs(Rw/w)<1)
      }
      if (kernel=='epan'){
        kweights = .75*(1-(Rw/w)^2)*(abs(Rw/w)<1)
      }
      
      ## Model adjustment
      
      if (p>0){
        X.adj = NULL
        Xk.adj = NULL
        if (evalat==''|evalat=='cutoff'){
          evall = cutoff
          evalr = cutoff
        } else {
          evall = mean(Rw[D==0])
          evalr = mean(Rw[D==1])
        }
        R.adj = Rw - Dw*evalr - (1-Dw)*evall
        
        for (k in 1:ncol(X)){
          Rpoly = poly(R.adj,order=p,raw=TRUE)
          lfit.t = lm(Xw[Dw==1,k] ~ Rpoly[Dw==1,],weights=kweights[Dw==1])
          Xk.adj[Dw==1] = lfit.t$residuals + lfit.t$coefficients[1]
          lfit.c = lm(Xw[Dw==0,k] ~ Rpoly[Dw==0,],weights=kweights[D==0])
          Xk.adj[Dw==0] = lfit.c$residuals + lfit.c$coefficients[1]
          X.adj = cbind(X.adj,Xk.adj)
        }
        Xw = X.adj
      }
      
      
      ## Statistics and p-values
      
      if (statistic=='hotelling'){
        aux = hotelT2(Xw,Dw)
        obs.stat = as.numeric(aux$statistic)
        if (approx==FALSE){
          stat.distr = array(NA,dim=c(reps,1))
          for (i in 1:reps){
            D.sample = sample(Dw,replace=FALSE)
            aux.sample = hotelT2(Xw,D.sample)
            obs.stat.sample = as.numeric(aux.sample$statistic)
            stat.distr[i] = obs.stat.sample
          }
          p.value = mean(abs(stat.distr) >= abs(obs.stat))
        } else {
          p.value = as.numeric(aux$p.value)
        }
        table.rdw[col,1] = p.value
      } else {
        aux = rdrandinf.model(Xw,Dw,statistic=statistic,kweights=kweights,pvalue=TRUE)
        obs.stat = as.numeric(aux$statistic)
        if (approx==FALSE){
          stat.distr = array(NA,dim=c(reps,ncol(X)))
          for (i in 1:reps){
            D.sample = sample(Dw,replace=FALSE)
            aux.sample = rdrandinf.model(Xw,D.sample,statistic=statistic,kweights=kweights)
            obs.stat.sample = as.numeric(aux.sample$statistic)
            stat.distr[i,] = obs.stat.sample
          }
          p.value = rowMeans(t(abs(stat.distr)) >= abs(obs.stat))
        } else {
          if (p==0){
            p.value = as.numeric(aux$p.value)
          } else {
            p.value = NULL
            for (k in 1:ncol(X)){
              lfit = lm(Xw[,k] ~ Dw + Rpoly + Dw*Rpoly,kweights=kweights)
              tstat = lfit$coefficients['Dw']/sqrt(vcov(lfit)['Dw','Dw'])
              p.value = c(p.value,2*pnorm(-abs(tstat)))
            }
          }
        }
        
        table.rdw[col,1] = min(p.value)
        tmp = which.min(p.value)
        table.rdw[col,2] = tmp

        if (!is.null(colnames(Xw)[tmp]) & colnames(Xw)[tmp]!='') {
          varname = substring(colnames(Xw)[tmp],1,15)
        } else {
          varname = tmp
        }
      }
     
    } else {
      table.rdw[col,1] = NA
      table.rdw[col,2] = NA
      varname = NA
    }
    
    if (quietly==FALSE){
      cat(paste0(format(toString(round(w,3)),width=25),
                 format(toString(round(table.rdw[col,1],3)),width=10),
                 format(toString(varname),width=20),
                 format(toString(round(table.rdw[col,3],3)),width=13),
                 format(toString(table.rdw[col,4]),width=7),
                 format(toString(table.rdw[col,5]),width=5)))
      cat('\n')
    }
    col = col + 1
    
  }
  
  
  #################################################################
  # Find recommended window
  #################################################################

  if (!missing(X)){
    Pvals = table.rdw[,1]
    if (all(Pvals>level)){
      tmp = length(Pvals)
    } else {
      tmp = min(which(Pvals<level))
      if (tmp==1){
        warning('Smallest window does not pass covariate test. Decrease smallest window or reduce level')
      } else {
        tmp = tmp - 1
      }
    }
    
    rec.length = window.list[tmp]
    rec.window = c(cutoff-rec.length,cutoff+rec.length)
    
    if (quietly==FALSE){
      cat('\n\n')
      cat(paste0('Recommended window is [',round(rec.window[1],3),';',round(rec.window[2],3),'] with ',table.rdw[tmp,4]+table.rdw[tmp,5],' observations (',
                 table.rdw[tmp,4],' below, ',table.rdw[tmp,5],' above).'))
      cat('\n\n')
    }
    
  } else {
    if(quietly==FALSE){warning('Need to specify covariates to find recommended length')}
    rec.window = NA
  }
  
  

  #################################################################
  # Plot p-values
  #################################################################
  
  if (plot==TRUE){
    if (!missing(X)){
      plot(window.list,Pvals)
    } else {
      stop('Cannot draw plot without covariates')
    }
  }
  
  
  #################################################################
  # Output
  #################################################################
  
  colnames(table.sumstats) = c('Left of c','Right of c')
  rownames(table.sumstats) = c('Number of obs','1th percentile','5th percentile','10th percentile','20th percentile')
  
  table.rdw = cbind(window.list,table.rdw)
  colnames(table.rdw)= c('W.length','p-value','Variable','Bi.test','Obs<c','Obs>=c')
  
  output = list(window = rec.window,
                results = table.rdw,
                summary = table.sumstats)

  return(output)
  
}