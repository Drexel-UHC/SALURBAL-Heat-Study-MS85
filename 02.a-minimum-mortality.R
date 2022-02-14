
# GENERATE THE MATRIX FOR STORING THE RESULTS MINUMUM MORTALITY TEMPERATURE
################################################################################
minperccity1 <- mintempcity1 <- rep(NA,length(datasetlist))
names(mintempcity1) <- names(minperccity1) <- cities$city


quants = sort(unique( c( (0:100)/100, 1-(1:20)/400, (1:20)/400 ) ))

RR.IN.PCT.1 = matrix(NA, nrow=131, ncol=length(datasetlist))

# DEFINE MINIMUM MORTALITY VALUES: EXCLUDE LOW AND VERY HOT TEMPERATURE
for(i in seq(length(datasetlist))) {
  cat(i,"")
  
  data = dailytemp[[i]] 
  these.knots =as.numeric(KNOTS1[i,]);  this.bound = as.numeric(BOUNDS1[i,])
  
  predvar <- quantile(data$tmean,(1:99)/100,na.rm=T)
  argvar <- list(x=predvar,fun=varfun,
                 knots=these.knots,#degree=vardegree, 
                 Bound=this.bound)
  bvar <- do.call(onebasis,argvar)
  
  predvar2 <- quantile(data$tmean,quants,na.rm=T)
  argvar2 <- list(x=predvar2,fun=varfun,
                 knots=these.knots,#degree=vardegree, 
                 Bound=this.bound)
  bvar2 <- do.call(onebasis,argvar2)
  
  # MINIMUM MORTALITY TEMPERATURE MV MODEL 1
  # REDEFINE THE FUNCTION USING ALL THE ARGUMENTS (BOUNDARY KNOTS INCLUDED)
  temp.pred= exp(bvar%*%blup1[[i]]$blup)
  minperccity1[i] <- (1:99)[which.min((temp.pred))]
  mintempcity1[i] <- quantile(data$tmean,minperccity1[i]/100,na.rm=T)
  
  RR.IN.PCT.1[,i] <- crosspred(bvar2,coef=blup1[[i]]$blup,vcov=blup1[[i]]$vcov,
                              model.link="log",cen=mean(data$tmean,na.rm=T), at=predvar2)$allRRfit
   

}

MMT = data.frame(nsalid=cities$nsalid1, MMT1=mintempcity1, MMT1perc=minperccity1)

 
 