if(nclusters == 18 ){ ClustersToUse = metadata$ECDFClusters_new23
   } else if(nclusters==12) {ClustersToUse = metadata$ECDFClusters_12
   } else if(nclusters==6) {ClustersToUse = metadata$ECDFClusters_6
   } else {print("error: must specify nclusters 18, 12 or 6")}
 


uniquelabels=sort(unique(ClustersToUse))
nmodels=5
# COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY FOR EACH MODEL
# model 1 
coef1 <- matrix(NA,length(uniquelabels),4,uniquelabels)
vcov1 <- vector("list",length(uniquelabels)) 
names(vcov1) <- uniquelabels 
red.1 <- vector("list",length(uniquelabels))
# model 2 
coef2 <- matrix(NA,length(uniquelabels),4,uniquelabels)
vcov2 <- vector("list",length(uniquelabels)) 
names(vcov2) <- uniquelabels 
red.2 <- vector("list",length(uniquelabels))
# model 3 
coef3 <- matrix(NA,length(uniquelabels),4,uniquelabels)
vcov3 <- vector("list",length(uniquelabels)) 
names(vcov3) <- uniquelabels 
red.3 <- vector("list",length(uniquelabels))
# model 4 
coef4 <- matrix(NA,length(uniquelabels),4,uniquelabels)
vcov4 <- vector("list",length(uniquelabels)) 
names(vcov4) <- uniquelabels 
red.4 <- vector("list",length(uniquelabels))
# model 5 
coef5 <- matrix(NA,length(uniquelabels),4,uniquelabels)
vcov5 <- vector("list",length(uniquelabels)) 
names(vcov5) <- uniquelabels 
red.5 <- vector("list",length(uniquelabels))

mintempcity1=rep(NA,length(uniquelabels))
mintempcity2=rep(NA,length(uniquelabels))
mintempcity3=rep(NA,length(uniquelabels))
mintempcity4=rep(NA,length(uniquelabels))
mintempcity5=rep(NA,length(uniquelabels))

minperccity1=rep(NA,length(uniquelabels))
minperccity2=rep(NA,length(uniquelabels))
minperccity3=rep(NA,length(uniquelabels))
minperccity4=rep(NA,length(uniquelabels))
minperccity5=rep(NA,length(uniquelabels))

CLUSTERKNOTS = matrix(NA, nrow=length(uniquelabels),ncol=length(input.knots))
CLUSTERBOUNDS = matrix(NA, nrow=length(uniquelabels),ncol=2)

RR.IN.PCT.1 = list()
RR.IN.PCT.2 = list()
RR.IN.PCT.3 = list()
RR.IN.PCT.4 = list()
RR.IN.PCT.5 = list()


#plotpath="C:/Users/bns48/HEAT/TempOutput/Respiratory-p10p75p90-Lag21/"
 

fitmat<- matrix(NA,length(uniquelabels), nmodels)
quants = sort(unique( c( (0:100)/100, 1-(1:20)/400, (1:20)/400 ) ))


pdf(file=paste(plotsfolder,"AllClusters_of",nclusters,".pdf"), width=15, height=3)
  par(mfrow=c(1,6))


for(i in uniquelabels){
print(i)
  city.ids=which(ClustersToUse==i)  
  foo=DATASET.LIST[city.ids]
  data.all=do.call(rbind.data.frame,foo)

  plot(metadata2$LongCalc,metadata2$LatCalc,col="gray",cex=.75, main=paste("Cluster ",i," of ", nclusters))
  points(metadata2$LongCalc[city.ids],metadata2$LatCalc[city.ids],col="red")
  
  data1 = data.all[ order(data.all$nsalid1, data.all$group.series, data.all$date),]

  these.knots =quantile(data1$tmean, input.knot.percentiles ) 
  this.bound = c(min(data1$tmean),max(data1$tmean))
  
  CLUSTERKNOTS[i,] = these.knots
  CLUSTERBOUNDS[i,] = this.bound
  
  formula <- death~cb 
  
  #to be used with predictions below
  predvar <- quantile(data1$tmean,(1:99)/100,na.rm=T)
  argvar1 <- list(x=predvar,fun=varfun,
                  knots=these.knots,#degree=vardegree, 
                  Bound=this.bound)
  bvar <- do.call(onebasis,argvar1)
  
  predvar2 <- quantile(data1$tmean,quants,na.rm=T)
  argvar2 <- list(x=predvar2,fun=varfun,
                  knots=these.knots,#degree=vardegree, 
                  Bound=this.bound)
  bvar2 <- do.call(onebasis,argvar2)
  
  #ohter variables for fitting
  data1$group.series2 = as.factor( paste(data1$nsalid1,data1$group.series,sep="") )
   data1 = subset(data1, select= -c(group.series))
   data1$group.series=data1$group.series2
  
  data1$thisage=droplevels(data1$thisage)
  data1$month  <- as.factor(months(data1$date))
  data1$year   <- as.factor(format(data1$date, format="%Y") )
  data1$dow    <- as.factor(weekdays(data1$date))
  data1$maletxt = as.factor(data1$male) 
  data1$stratum1 <- as.factor(data1$year:data1$month:data1$dow:data1$thisage:data1$maletxt)
  data1$stratum = as.factor(paste(data1$nsalid1,data1$stratum1, sep=""))
  

  # MODEL 1, All ages
  data=data1
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
          error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in cluster=",i, ", Allages"))})
  model.all=model
  m1 = list(red=red,fit=mod.fit); 
  coef1[i,] <- coef(m1$red) ; vcov1[[i]] <- vcov(m1$red)
  fitmat[i,1] <- m1$fit     ; red.1[[i]] <- m1$red
  
  # MINIMUM MORTALITY TEMPERATURE MV MODEL 1
  # REDEFINE THE FUNCTION USING ALL THE ARGUMENTS (BOUNDARY KNOTS INCLUDED)
  temp.pred= exp(bvar%*%coef1[i,])
  minperccity1[i] <- (1:99)[which.min((temp.pred))]
  mintempcity1[i] <- quantile(data$tmean,minperccity1[i]/100,na.rm=T)
  RR.IN.PCT.1[[i]] <- crosspred(bvar2,coef=coef1[i,],vcov=vcov1[[i]],
                               model.link="log",cen=mintempcity1[i], at=predvar2)
  plot(RR.IN.PCT.1[[i]], ptype="overall", main=paste("All ages; cluster ", i,"of ",nclusters),ylim=c(0,2), xlab="Temperature (C)", ylab="RR vs. MMT")
  points(predvar[c(1,99,1,99)],c(1,1,0,0), col="red")
  points(predvar[c(5,95,5,95)],c(1,1,0,0), col="blue")
  add.histogram(data$tmean)

  # MODEL 2, Age 65+
  data=subset(data1, thisage == '65+')
  data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
          error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in cluster=",i, ", 65+"))})
  model.65plus=model
  m2 = list(red=red,fit=mod.fit); 
  coef2[i,] <- coef(m2$red) ; vcov2[[i]] <- vcov(m2$red)
  fitmat[i,2] <- m2$fit     ; red.2[[i]] <- m2$red
  temp.pred= exp(bvar%*%coef2[i,])
  minperccity2[i] <- (1:99)[which.min((temp.pred))]
  mintempcity2[i] <- quantile(data$tmean,minperccity2[i]/100,na.rm=T)
  RR.IN.PCT.2[[i]] <- crosspred(bvar2,coef=coef2[i,],vcov=vcov2[[i]],
                                model.link="log",cen=mintempcity2[i], at=predvar2)
  plot(RR.IN.PCT.2[[i]], ptype="overall", main=paste("Age 65+; cluster ", i,"of ",nclusters),ylim=c(0,2), xlab="Temperature (C)", ylab="RR vs. MMT")
  points(predvar[c(1,99,1,99)],c(1,1,0,0), col="red")
  points(predvar[c(5,95,5,95)],c(1,1,0,0), col="blue")
  add.histogram(data$tmean)
  
  # MODEL 3, Age < 65
  data=subset(data1, thisage != '65+')
  data$group.series=droplevels(data$group.series)
    tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
          error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in cluster=",i, ", <65"))})
  model.less65=model
  m3 = list(red=red,fit=mod.fit); 
  coef3[i,] <- coef(m3$red) ; vcov3[[i]] <- vcov(m3$red)
  fitmat[i,3] <- m3$fit     ; red.3[[i]] <- m3$red
  temp.pred= exp(bvar%*%coef3[i,])
  minperccity3[i] <- (1:99)[which.min((temp.pred))]
  mintempcity3[i] <- quantile(data$tmean,minperccity3[i]/100,na.rm=T)
  RR.IN.PCT.3[[i]] <- crosspred(bvar2,coef=coef3[i,],vcov=vcov3[[i]],
                                model.link="log",cen=mintempcity3[i], at=predvar2)
  plot(RR.IN.PCT.3[[i]], ptype="overall", main=paste("Age <65; cluster ", i,"of ",nclusters),ylim=c(0,2), xlab="Temperature (C)", ylab="RR vs. MMT")
  points(predvar[c(1,99,1,99)],c(1,1,0,0), col="red")
  points(predvar[c(5,95,5,95)],c(1,1,0,0), col="blue")
  add.histogram(data$tmean)
  
  # MODEL 4, Age 50-64
  data=subset(data1, thisage == '50-64')
  data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
          error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error cluster=",i, ", 50-64"))})
  model.50to64=model
  m4 = list(red=red,fit=mod.fit); 
  coef4[i,] <- coef(m4$red) ; vcov4[[i]] <- vcov(m4$red)
  fitmat[i,4] <- m4$fit     ; red.4[[i]] <- m4$red
  temp.pred= exp(bvar%*%coef4[i,])
  minperccity4[i] <- (1:99)[which.min((temp.pred))]
  mintempcity4[i] <- quantile(data$tmean,minperccity4[i]/100,na.rm=T)
  RR.IN.PCT.4[[i]] <- crosspred(bvar2,coef=coef4[i,],vcov=vcov4[[i]],
                                model.link="log",cen=mintempcity4[i], at=predvar2)
  plot(RR.IN.PCT.4[[i]], ptype="overall", main=paste("Age 50-64; cluster ", i,"of ",nclusters),ylim=c(0,2), xlab="Temperature (C)", ylab="RR vs. MMT")
  points(predvar[c(1,99,1,99)],c(1,1,0,0), col="red")
  points(predvar[c(5,95,5,95)],c(1,1,0,0), col="blue")
  add.histogram(data$tmean)
  
  # MODEL 5, Age <50 
  data=subset(data1, thisage == '0-49')
  data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
          error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in cluster=",i, ", 0-49"))})
  model.0to49=model
  m5 = list(red=red,fit=mod.fit); 
  coef5[i,] <- coef(m5$red) ; vcov5[[i]] <- vcov(m5$red)
  fitmat[i,5] <- m5$fit     ; red.5[[i]] <- m5$red
  temp.pred= exp(bvar%*%coef5[i,])
  minperccity5[i] <- (1:99)[which.min((temp.pred))]
  mintempcity5[i] <- quantile(data$tmean,minperccity5[i]/100,na.rm=T)
  RR.IN.PCT.5[[i]] <- crosspred(bvar2,coef=coef5[i,],vcov=vcov5[[i]],
                                model.link="log",cen=mintempcity5[i], at=predvar2)
  plot(RR.IN.PCT.5[[i]], ptype="overall", main=paste("Age <50; cluster ", i,"of ",nclusters),ylim=c(0,2), xlab="Temperature (C)", ylab="RR vs. MMT")
  points(predvar[c(1,99,1,99)],c(1,1,0,0), col="red")
  points(predvar[c(5,95,5,95)],c(1,1,0,0), col="blue")
  add.histogram(data$tmean)
  
#Save the entire model for the future - will be needed to construct lags later
#if(saveallraw=="Y"){ 
#  save(data1,model.all,model.65plus,model.less65,model.50to64,model.0to49,m1,m2,m3,m4,m5,varfun,these.knots,this.bound,lag,lagnk, 
#       file=paste(rawoutput,"City_",cities$city[i],".Rdata", sep="") )
#}

#rm(model.all,model.65plus,model.less65,model.50to64,model.0to49,m1,m2,m3,m4,m5)
  
  
} 

dev.off()


MV.ALLAGES = list()
MV.65PLUS = list()
MV.LESS65 = list()
MV.50TO64 = list()
MV.0TO49 = list()


MV.ALLAGES$BLUP1 = list()
MV.65PLUS$BLUP1 = list()
MV.LESS65$BLUP1 = list()
MV.50TO64$BLUP1 = list()
MV.0TO49$BLUP1= list()

MV.ALLAGES$MMT$MMTPERC = rep(NA,326)
MV.65PLUS$MMT$MMTPERC = rep(NA,326)
MV.LESS65$MMT$MMTPERC =  rep(NA,326)
MV.50TO64$MMT$MMTPERC = rep(NA,326)
MV.0TO49$MMT$MMTPERC =  rep(NA,326)

MV.ALLAGES$MMT$MMT1=  rep(NA,326)
MV.65PLUS$MMT$MMT1 =  rep(NA,326)
MV.LESS65$MMT$MMT1 =  rep(NA,326)
MV.50TO64$MMT$MMT1 =  rep(NA,326)
MV.0TO49$MMT$MMT1 =  rep(NA,326)

KNOTSFORCITY = matrix(NA, nrow=326, ncol=length(input.knots))
BOUNDSFORCITY = matrix(NA, nrow=326,ncol=2)
for(i in uniquelabels){
  print(i)
  city.ids=which(ClustersToUse==i)  #final 18

  for(j in city.ids){
    MV.ALLAGES$BLUP1[[j]]=list()
    MV.ALLAGES$BLUP1[[j]]$blup = red.1[[i]]$coefficients
    MV.ALLAGES$BLUP1[[j]]$vcov = red.1[[i]]$vcov
    MV.ALLAGES$MMT$MMT1[city.ids] = mintempcity1[i]
    MV.ALLAGES$MMT$MMTPERC[city.ids] = minperccity1[i]
    
    MV.65PLUS$BLUP1[[j]]=list()
    MV.65PLUS$BLUP1[[j]]$blup = red.2[[i]]$coefficients
    MV.65PLUS$BLUP1[[j]]$vcov = red.2[[i]]$vcov
    MV.65PLUS$MMT$MMT1[city.ids] = mintempcity2[i]
    MV.65PLUS$MMT$MMTPERC[city.ids] = minperccity2[i]
    
    MV.LESS65$BLUP1[[j]]=list()
    MV.LESS65$BLUP1[[j]]$blup = red.3[[i]]$coefficients
    MV.LESS65$BLUP1[[j]]$vcov = red.3[[i]]$vcov
    MV.LESS65$MMT$MMT1[city.ids] = mintempcity3[i]
    MV.LESS65$MMT$MMTPERC[city.ids] = minperccity3[i]
    
    MV.50TO64$BLUP1[[j]]=list()
    MV.50TO64$BLUP1[[j]]$blup = red.4[[i]]$coefficients
    MV.50TO64$BLUP1[[j]]$vcov = red.4[[i]]$vcov
    MV.50TO64$MMT$MMT1[city.ids] = mintempcity4[i]
    MV.50TO64$MMT$MMTPERC[city.ids] = minperccity4[i]
    
    MV.0TO49$BLUP1[[j]]=list()
    MV.0TO49$BLUP1[[j]]$blup = red.5[[i]]$coefficients
    MV.0TO49$BLUP1[[j]]$vcov = red.5[[i]]$vcov
    MV.0TO49$MMT$MMT1[city.ids] = mintempcity5[i]
    MV.0TO49$MMT$MMTPERC[city.ids] = minperccity5[i]
    
    KNOTSFORCITY[j,] =  CLUSTERKNOTS[i,]
    BOUNDSFORCITY[j,] =  CLUSTERBOUNDS[i,]
    
  }
} 
  


