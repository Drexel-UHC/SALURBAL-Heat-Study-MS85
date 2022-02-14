#FUNCTION TO READ IN SAS DATASET
read.my.data = function(datafilename, subset=FALSE, strata=NULL){ 
  #values for strata should be in quotes, like: "thisage=='65+' & male==1"
  foo=read.sas7bdat(datafilename) 
  foo$death = foo$deaths
  foo$tmean = foo$ADtemp_pw
  foo$logpop = log(foo$pop_count/1000) #need to verify if population counts are in # of people or 1000's of people
  foo$date= as.Date(foo$allDate, origin="1960-01-01")
  foo$dow=weekdays(foo$date)
  foo$year=format(foo$date,"%Y") 
  
  # keep only the rows we need
  if(subset==TRUE){ foo = subset(foo, return(eval(parse(text=strata)))) }
  
  # keep only the variables we need
  foo = subset(foo, is.na(tmean)==FALSE , select=varkeep )  
  
  #return:
  foo
} 

# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}
 


# FUNCTION FOR COMPUTING THE P-VALUE OF A WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}


# FUNCTION TO ADD HISTOGRAM OF EXPOSURE
add.histogram=function(x){
  breaks <- c(min(x,na.rm=T)-1,seq(min(x,na.rm=T),
                                 max(x,na.rm=T),length=30),max(x,na.rm=T)+1)
  hist.a <- hist(x,breaks=breaks,plot=F)
  hist.a$density <- hist.a$density/max(hist.a$density)*0.7
  prop <- max(hist.a$density)/max(hist.a$counts)
  counts <- pretty(hist.a$count,3)
  plot(hist.a,ylim=c(0,max(hist.a$density)*3.5),axes=F,ann=F,col=grey(0.95),
     breaks=breaks,freq=F,add=T)
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
}


# FUNCTION TO ADD HISTOGRAM OF EXPOSURE
add.histogram.color=function(x,mycol=grey(0.95)){
  breaks <- c(min(x,na.rm=T)-1,seq(min(x,na.rm=T),
                                 max(x,na.rm=T),length=30),max(x,na.rm=T)+1)
  hist.a <- hist(x,breaks=breaks,plot=F)
  hist.a$density <- hist.a$density/max(hist.a$density)*0.7
  prop <- max(hist.a$density)/max(hist.a$counts)
  counts <- pretty(hist.a$count,3)
  plot(hist.a,ylim=c(0,max(hist.a$density)*3.5),axes=F,ann=F,col=mycol,
     breaks=breaks,freq=F,add=T)
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7)
}


# FUNCTION TO ADD HISTOGRAMS TO A PAIRS PLOT
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}