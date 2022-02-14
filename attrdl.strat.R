###
### (c) Antonio Gasparrini 2015-2017
#
################################################################################
# FUNCTION FOR COMPUTING ATTRIBUTABLE MEASURES FROM DLNM
#   REQUIRES dlnm VERSION 2.2.0 AND ON
################################################################################
#
# DISCLAIMER:
#   THE CODE COMPOSING THIS FUNCTION HAS NOT BEEN SYSTEMATICALLY TESTED. THE
#   PRESENCE OF BUGS CANNOT BE RULED OUT. ALSO, ALTHOUGH WRITTEN GENERICALLY
#   FOR WORKING IN DIFFERENT SCENARIOS AND DATA, THE FUNCTION HAS NOT BEEN
#   TESTED IN CONTEXTS DIFFERENT THAN THE EXAMPLE INCLUDED IN THE PAPER.
#   IT IS RESPONSIBILITY OF THE USER TO CHECK THE RELIABILITY OF THE RESULTS IN
#   DIFFERENT APPLICATIONS.
#
# Version: 25 January 2017
# AN UPDATED VERSION CAN BE FOUND AT:
#   https://github.com/gasparrini/2014_gasparrini_BMCmrm_Rcodedata
#
################################################################################
# SEE THE PDF WITH A DETAILED DOCUMENTATION AT www.ag-myresearch.com
#
#   - x: AN EXPOSURE VECTOR OR (ONLY FOR dir="back") A MATRIX OF LAGGED EXPOSURES
#   - basis: THE CROSS-BASIS COMPUTED FROM x
#   - cases: THE CASES VECTOR OR (ONLY FOR dir="forw") THE MATRIX OF FUTURE CASES
#   - model: THE FITTED MODEL
#   - coef AND vcov: COEF AND VCOV FOR basis IF model IS NOT PROVIDED
#   - type: EITHER "an" OR "af" FOR ATTRIBUTABLE NUMBER OR FRACTION
#   - dir: EITHER "back" OR "forw" FOR BACKWARD OR FORWARD PERSPECTIVES
#   - tot: IF TRUE, THE TOTAL ATTRIBUTABLE RISK IS COMPUTED
#   - cen: THE REFERENCE VALUE USED AS COUNTERFACTUAL SCENARIO
#   - range: THE RANGE OF EXPOSURE. IF NULL, THE WHOLE RANGE IS USED
#   - sim: IF SIMULATION SAMPLES SHOULD BE RETURNED. ONLY FOR tot=TRUE
#   - nsim: NUMBER OF SIMULATION SAMPLES
#   - strat.by:  columns the same length as cases, that
#           define the groups within which cases belong, e.g., age and sex
################################################################################
attrdl.strat <- function(x,basis,cases,model=NULL,coef=NULL,vcov=NULL,type="af",
  dir="back",tot=TRUE,cen,range=NULL,sim=FALSE,nsim=5000, strat.by=NULL, collapsed=TRUE, f.summary=TRUE) {
################################################################################
#
  # CHECK VERSION OF THE DLNM PACKAGE
  if(packageVersion("dlnm")<"2.2.0") 
    stop("update dlnm package to version >= 2.2.0")
#
  # EXTRACT NAME AND CHECK type AND dir
  name <- deparse(substitute(basis))
  type <- match.arg(type,c("an","af"))
  dir <- match.arg(dir,c("back","forw"))
#
  # DEFINE CENTERING
  if(missing(cen) && is.null(cen <- attr(basis,"argvar")$cen))
    stop("'cen' must be provided")
  if(!is.numeric(cen) && length(cen)>1L) stop("'cen' must be a numeric scalar")
  attributes(basis)$argvar$cen <- NULL
#  
  # SELECT RANGE (FORCE TO CENTERING VALUE OTHERWISE, MEANING NULL RISK)
  if(!is.null(range)) x[x<range[1]|x>range[2]] <- cen
#
  # COMPUTE THE MATRIX OF
  #   - LAGGED EXPOSURES IF dir="back"
  #   - CONSTANT EXPOSURES ALONG LAGS IF dir="forw"
  lag <- attr(basis,"lag")
  if(NCOL(x)==1L) {
    at <- if(dir=="back") tsModel:::Lag(x,seq(lag[1],lag[2])) else 
      matrix(rep(x,diff(lag)+1),length(x))
  } else {
    if(dir=="forw") stop("'x' must be a vector when dir='forw'")
    if(ncol(at <- x)!=diff(lag)+1) 
      stop("dimension of 'x' not compatible with 'basis'")
  }
#
  # NUMBER USED FOR THE CONTRIBUTION AT EACH TIME IN FORWARD TYPE
  #   - IF cases PROVIDED AS A MATRIX, TAKE THE ROW AVERAGE
  #   - IF PROVIDED AS A TIME SERIES, COMPUTE THE FORWARD MOVING AVERAGE
  #   - THIS EXCLUDES MISSING ACCORDINGLY
  # ALSO COMPUTE THE DENOMINATOR TO BE USED BELOW
  cases.long=cases
if(is.null(strat.by)){
  if(NROW(cases)!=NROW(at)) stop("'x' and 'cases' not consistent")
  if(NCOL(cases)>1L) {
    if(dir=="back") stop("'cases' must be a vector if dir='back'")
    if(ncol(cases)!=diff(lag)+1) stop("dimension of 'cases' not compatible")
    den <- sum(rowMeans(cases,na.rm=TRUE),na.rm=TRUE)
    cases <- rowMeans(cases)
  } else {
    den <- sum(cases,na.rm=TRUE) 
    if(dir=="forw") 
      cases <- rowMeans(as.matrix(tsModel:::Lag(cases,-seq(lag[1],lag[2]))))
  }
} else {
    groups=unique(strat.by)
    strata=dim(groups)[1]
    cases.list=list()
    for(k in 1:strata){
      cases.list[[k]]=list()
      cases.list[[k]]$rows=which(strat.by[,1] == groups[k,1] & strat.by[,2] == groups[k,2] )
      cases.list[[k]]$cases=cases.long[ cases.list[[k]]$rows ]  
      cases.list[[k]]$den = sum(cases.list[[k]]$cases, na.rm=TRUE )
      cases.list[[k]]$cases.sum.lag = rowMeans(as.matrix(tsModel:::Lag(cases.list[[k]]$cases, -seq(lag[1],lag[2]))))   
    }
}
  
################################################################################
#
  # EXTRACT COEF AND VCOV IF MODEL IS PROVIDED
  if(!is.null(model)) {
    cond <- paste0(name,"[[:print:]]*v[0-9]{1,2}\\.l[0-9]{1,2}")
    if(ncol(basis)==1L) cond <- name
    model.class <- class(model)
    coef <- dlnm:::getcoef(model,model.class)
    ind <- grep(cond,names(coef))
    coef <- coef[ind]
    vcov <- dlnm:::getvcov(model,model.class)[ind,ind,drop=FALSE]
    model.link <- dlnm:::getlink(model,model.class)
    if(model.link!="log") stop("'model' must have a log link function")
  }
#
  # IF REDUCED ESTIMATES ARE PROVIDED
  typebasis <- ifelse(length(coef)!=ncol(basis),"one","cb")
#
################################################################################
#
  # PREPARE THE ARGUMENTS FOR TH BASIS TRANSFORMATION
  predvar <- if(typebasis=="one") x else seq(NROW(at))
  predlag <- if(typebasis=="one") 0 else dlnm:::seqlag(lag)
#  
  # CREATE THE MATRIX OF TRANSFORMED CENTRED VARIABLES (DEPENDENT ON typebasis)
  if(typebasis=="cb") {
    Xpred <- dlnm:::mkXpred(typebasis,basis,at,predvar,predlag,cen)
    Xpredall <- 0
    for (i in seq(length(predlag))) {
      ind <- seq(length(predvar))+length(predvar)*(i-1)
      Xpredall <- Xpredall + Xpred[ind,,drop=FALSE]
    }
  } else {
    basis <- do.call(onebasis,c(list(x=x),attr(basis,"argvar")))
    Xpredall <- dlnm:::mkXpred(typebasis,basis,x,predvar,predlag,cen)
  }
#  
  # CHECK DIMENSIONS  
  if(length(coef)!=ncol(Xpredall))
    stop("arguments 'basis' do not match 'model' or 'coef'-'vcov'")
  if(any(dim(vcov)!=c(length(coef),length(coef)))) 
    stop("arguments 'coef' and 'vcov' do no match")
  if(typebasis=="one" && dir=="back")
    stop("only dir='forw' allowed for reduced estimates")
#
################################################################################
#
  # COMPUTE AF AND AN 
  af <- 1-exp(-drop(as.matrix(Xpredall%*%coef)))
if(is.null(strat.by)){
  an <- af*cases
  # TOTAL
  #   - SELECT NON-MISSING OBS CONTRIBUTING TO COMPUTATION
  #   - DERIVE TOTAL AF
  #   - COMPUTE TOTAL AN WITH ADJUSTED DENOMINATOR (OBSERVED TOTAL NUMBER)
  if(tot) {
    isna <- is.na(an)
    af <- sum(an[!isna])/sum(cases[!isna])  #average of attributable fraction across rows
    an <- af*den   #apply average af to obtain grand total without missing rows
    }
} else {
    an.rows=list()
    an=list()
    for(k in 1:strata){  
      an.rows[[k]] = af[ cases.list[[k]]$rows]*cases.list[[k]]$cases.sum.lag 
      an[[k]] = an.rows[[k]]
    }
    if(tot){
      af=data.frame(groups)
      an=data.frame(groups)
      an$an=NULL
      af$af=NULL
      an$totdeaths=NULL
      af$totdeaths=NULL
      for(k in 1:strata){
      isna = is.na( an.rows[[k]])
      af$af[k] = sum( an.rows[[k]][!isna] ) / sum( cases.list[[k]]$cases.sum.lag[!isna] )
      an$an[k] = af$af[k] * cases.list[[k]]$den  
      an$totdeaths[k] = af$totdeaths[k] = cases.list[[k]]$den 
      }
      if(collapsed & !sim) {
        an=sum(an$an)
        af=mean(af$af)
      }
    }
  }
################################################################################
#
  # EMPIRICAL CONFIDENCE INTERVALS
  if(!tot && sim) {
    sim <- FALSE
    warning("simulation samples only returned for tot=T")
  }
  if(sim) {
    # SAMPLE COEF
    j <- length(coef)
    eigen <- eigen(vcov)
    X <- matrix(rnorm(length(coef)*nsim),nsim)
    coefsim <- coef + eigen$vectors %*% diag(sqrt(eigen$values),j) %*% t(X)
    # RUN THE LOOP
    if(is.null(strat.by)){
      afsim <- apply(coefsim,2, function(coefi) {
                                  ani <- (1-exp(-drop(Xpredall%*%coefi)))*cases
                                  sum(ani[!is.na(ani)])/sum(cases[!is.na(ani)])
                                  }
               )
      ansim <- afsim*den
    } else{
      afsim <- apply(coefsim,2, function(coefi, c.list=cases.list, nstrat=strata) {
                                 af.rows.i <- (1-exp(-drop(Xpredall%*%coefi)))
                                 an.rows.i=list()
                                 af.i = rep(NA, strata)
                                 for(k in 1:strata){  
                                   an.rows.i[[k]] = af.rows.i[c.list[[k]]$rows]*c.list[[k]]$cases.sum.lag 
                                   isna.i = is.na( an.rows.i[[k]])
                                   af.i[k] = sum( an.rows.i[[k]][!isna] ) / sum( cases.list[[k]]$cases.sum.lag[!isna] )
                                   } 
                                 af.i
                                 }
      )
    #  print(head(afsim))
      ansim = afsim * af$totdeaths
    }

    if(!is.null(strat.by)){if(strata==1){collapsed=FALSE}}

    if(collapsed){
    #  print(head(afsim))
      ansim = apply(ansim,2,sum)
      afsim = apply(afsim,2,mean)
    #  print(head(afsim))
    }
    
  }
#
################################################################################
#

  res <- if(sim) {
    if(f.summary){
      list(city.summary=c( an.sim.mean=mean(ansim), an.sim.se=sqrt(var(ansim)), an.low=quantile(ansim, 0.025), an.high=quantile(ansim, 0.975),
                           af.sim.mean = mean(afsim), af.sim.se=sqrt(var(afsim)), af.low=quantile(afsim, 0.025), af.high=quantile(afsim, 0.975)),
           afsim=afsim, ansim=ansim) 
    }else{
    if(type=="an") ansim else afsim}
  } else {
    if(f.summary){
      c(an=an, af=af)
    } else {
      if(type=="an") an else af }
       
  }
#
  return(res)
}

#
