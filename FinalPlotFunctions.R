
################################################################################
#  
plotcities.final = function(blup, mintempcity, age.grp, one.per.page="Y",subset.cities="allCities",subgroup.cities=cities.to.run,other.label=NULL, layout.rc = c(2,3), width.page=7.5, height.page=6.5, ...){

  plotsfolder.age= paste(plotsfolder,age.grp,"/",sep="")
  
  xlab <- expression(paste("Temperature (",degree,"C)"))
  if( one.per.page !="Y" ){
    pdf(paste(plotsfolder.age,scenario.running,"Temp_mortality_associations_",age.grp,subset.cities,".pdf",sep=""),width=width.page,height=height.page)
        par(mfrow=layout.rc)
  } else {
    pdf(paste(plotsfolder.age,scenario.running,"Temp_mortality_associations_",age.grp,subset.cities,".pdf",sep=""),width=3,height=4.25)
        par(mfrow=c(1,1))
            }
      
 for(i in subgroup.cities  ) {
  data <- dailytemp[[i]]
  these.knots =as.numeric(KNOTS1[i,]); this.bound = BOUNDS1[i,]
  
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean,fun=varfun,knots=these.knots, Boundary.knots=this.bound)
  
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
                    model.link="log",by=0.1,cen=mintempcity[i])
  
#  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
#   main= metadata2$City_link[i], bty="n" )
 
cex.main.size=1
place.temp=0.5
main.for.plot = paste(metadata2$City_link[i],", ",metadata2$country.name[i] , sep="")
   if(metadata2$length_name[i]>33){main.for.plot = paste(metadata2$City_link[i],", \n",metadata2$country.name[i] , sep="")
                                   place.temp=0}
   if(metadata2$length_name[i]>40){cex.main.size=.96}
   if(metadata2$length_name[i]>44){cex.main.size=.92}

  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab=" ",
       main=main.for.plot , cex.main=cex.main.size, bty="n" )
title(ylab="RR" , adj=.7)#, line=-.05)
  ind1 <- pred$predvar<=mintempcity[i]
  ind2 <- pred$predvar>=mintempcity[i]
  lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)

  #mtext( paste("Average temperature ", round(metadata2$mean[i],digits=1),cex=0.7,line=0)
  mtext( paste("Mean temp. ",round(metadata2$mean[i],digits=1) , "C", sep="") ,cex=0.7,line=place.temp)
  if( !is.null(other.label)){mtext(other.label[i],cex=0.7,line=-1)}
   axis(2,at=c(0.8, 1, 1.25, 1.5,   2), las=2) 
    axis(2,at=c(2.2),label="RR", las=2) 
axis(1)

  add.histogram.final(data$tmean) 
   
  abline(v=mintempcity[i],lty=3, lwd=2) 
  abline(v=c(metadata2$p5[i], metadata2$p95[i] ),lty=2)
  abline(v=c(metadata2$p1[i], metadata2$p99[i] ),lty=4)
 } 
 dev.off()
} 
#

plotcities.final.one.per.file = function(blup, mintempcity, age.grp, subset.cities="allCities",subgroup.cities=cities.to.run,other.label=NULL, height.page=680, width.page=480, units.page="px", ...){
  xlab <- expression(paste("Temperature (",degree,"C)"))

  plotsfolder.age= paste(plotsfolder,age.grp,"/City_by_city/",sep="")
   
 for(i in subgroup.cities  ) {
  data <- dailytemp[[i]]
  these.knots =as.numeric(KNOTS1[i,]); this.bound = BOUNDS1[i,]
  
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean,fun=varfun,knots=these.knots, Boundary.knots=this.bound)
  
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
                    model.link="log",by=0.1,cen=mintempcity[i])
  
#  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
#   main= metadata2$City_link[i], bty="n" )
 
#  png(paste(plotsfolder.age,scenario.running,"Temp_mortality_association_",age.grp,"_",metadata$SALID1[i],".pdf",sep=""),width=480,height=680)
  png(paste(plotsfolder.age,age.grp,"_",metadata$SALID1[i],".png",sep=""),width=width.page,height=height.page ,units=units.page)

  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab=" ",
       main=paste(metadata2$City_link[i],", ",metadata2$country.name[i] , sep="") , bty="n" )
title(ylab="RR" , adj=.7)#, line=-.05)
  ind1 <- pred$predvar<=mintempcity[i]
  ind2 <- pred$predvar>=mintempcity[i]
  lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)

  #mtext( paste("Average temperature ", round(metadata2$mean[i],digits=1),cex=0.7,line=0)
  mtext( paste("Mean temp. ",round(metadata2$mean[i],digits=1) , "C", sep="") ,cex=0.7,line=.5)
  if( !is.null(other.label)){mtext(other.label[i],cex=0.7,line=-1)}
   axis(2,at=c(0.8, 1, 1.25, 1.5,   2), las=2) 
    axis(2,at=c(2.2),label="RR", las=2) 
axis(1)

  add.histogram.final(data$tmean) 
   
  abline(v=mintempcity[i],lty=3, lwd=2) 
  abline(v=c(metadata2$p5[i], metadata2$p95[i] ),lty=2)
  abline(v=c(metadata2$p1[i], metadata2$p99[i] ),lty=4)

 dev.off()

 } 
} 
#



add.histogram.final=function(x){
  breaks <- c(min(x,na.rm=T)-1,seq(min(x,na.rm=T),
                                   max(x,na.rm=T),length=30),max(x,na.rm=T)+1)
  hist.a <- hist(x,breaks=breaks,plot=F)
  hist.a$density <- hist.a$density/max(hist.a$density)*0.7
  prop <- max(hist.a$density)/max(hist.a$counts)
  counts <- pretty(hist.a$count,3)
  plot(hist.a,ylim=c(0,max(hist.a$density)*3.5),axes=F,ann=F,col=grey(0.95),
       breaks=breaks,freq=F,add=T)
  axis(4,at=counts*prop,labels=counts,cex.axis=0.7, las=2)
}

################################################################################
#  
plotcities = function(blup, mintempcity, age.grp, one.per.page="Y",subset.cities="allCities",subgroup.cities=cities.to.run,other.label=NULL, ...){

  plotsfolder.age= paste(plotsfolder,age.grp,"/",sep="")
  
  xlab <- expression(paste("Temperature (",degree,"C)"))
  if( one.per.page !="Y" ){
    pdf(paste(plotsfolder.age,scenario.running,"Temp_mortality_associations_",age.grp,subset.cities,".pdf",sep=""),width=8.5,height=11)
        par(mfrow=c(4,4))
  } else {
    pdf(paste(plotsfolder.age,scenario.running,"Temp_mortality_associations_",age.grp,subset.cities,".pdf",sep=""),width=3,height=3)
        par(mfrow=c(1,1))
            }
      
 for(i in subgroup.cities  ) {
  data <- dailytemp[[i]]
  these.knots =as.numeric(KNOTS1[i,]); this.bound = BOUNDS1[i,]
  
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  argvar <- list(x=data$tmean,fun=varfun,knots=these.knots, Boundary.knots=this.bound)
  
  bvar <- do.call(onebasis,argvar)
  pred <- crosspred(bvar,coef=blup[[i]]$blup,vcov=blup[[i]]$vcov,
                    model.link="log",by=0.1,cen=mintempcity[i])
  
  plot(pred,type="n",ylim=c(0,2),yaxt="n",lab=c(6,5,7),xlab=xlab,ylab="RR",
    #   main=paste(metadata2$City_link[i],", ",metadata2$country.name[i] , sep=""))
  main= metadata2$City_link[i], bty="n" )

  ind1 <- pred$predvar<=mintempcity[i]
  ind2 <- pred$predvar>=mintempcity[i]
  lines(pred$predvar[ind1],pred$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred$predvar[ind2],pred$allRRfit[ind2],col=2,lwd=1.5)
  mtext(metadata2$country.name[i],cex=0.7,line=0)
  
  if( !is.null(other.label)){mtext(other.label[i],cex=0.7,line=-1)}
  #axis(1,at=-8:8*5)
  #axis(2,at=1:5*0.5) 
  axis(2,at=c(0.8, 1, 1.25, 1.5,   2), las=2) 
 
  add.histogram.final(data$tmean) 
   
  abline(v=mintempcity[i],lty=3) 
  abline(v=c(metadata2$p5[i], metadata2$p95[i] ),lty=2)
  abline(v=c(metadata2$p1[i], metadata2$p99[i] ),lty=4)
 } 
 dev.off()
} 
#

plot.by.list=function(mylist,...){
  plotcities(blup=mylist$blup, mintempcity=mylist$mintempcity, age.grp=mylist$age.grp, one.per.page="N",
             subset.cities="_byCountry")
  
  foo=aggregate(metadata2$p50~metadata2$ECDFClusters_12,FUN=mean)
  clusterID=foo$`metadata2$ECDFClusters_12`[order(foo$`metadata2$p50`)]
  bycluster=NULL
  for(j in clusterID){
    bycluster=c(bycluster,which(metadata2$ECDFClusters_12==j))
  }
  plotcities(blup=mylist$blup, mintempcity=mylist$mintempcity, age.grp=mylist$age.grp, one.per.page="N",
             subset.cities="_bycluster",subgroup.cities=bycluster, other.label = paste("Cluster", metadata2$ECDFClusters_12))
  
  plotcities(blup=mylist$blup, mintempcity=mylist$mintempcity, age.grp=mylist$age.grp, one.per.page="N",
             subset.cities="_byRange",subgroup.cities=order(metadata2$range), other.label = paste("Temperature Range=", round(metadata2$range,digits=1 )))
  
  Zone=sort(unique(metadata2$KPLevel1_Precip))
  byzone=NULL
  for(j in Zone){
    byzone=c(byzone,which(metadata2$KPLevel1_Precip==j))
  }
  plotcities(blup=mylist$blup, mintempcity=mylist$mintempcity, age.grp=mylist$age.grp, one.per.page="N",
             subset.cities="_byKPLevel1_Precip",subgroup.cities=byzone, other.label = metadata2$KPLevel1_Precip )
}



plot.city.oneage = function(pred.i=pred, data.i=data, mintemp.i=mintemp, vlines.i=c(metadata2$p5[i], metadata2$p95[i],metadata2$p1[i], metadata2$p99[i]  ), age.grp="Unknown Age", ...){
  plot(pred.i,type="n",ylim=c(0,2),yaxt="n",axes=FALSE,lab=c(6,5,7),xlab=xlab,ylab="RR", bty="n")#, 
       #main=age.grp  )
  ind1 <- pred.i$predvar<=mintemp.i
  ind2 <- pred.i$predvar>=mintemp.i
  lines(pred.i$predvar[ind1],pred.i$allRRfit[ind1],col=4,lwd=1.5)
  lines(pred.i$predvar[ind2],pred.i$allRRfit[ind2],col=2,lwd=1.5)
  
  mtext(age.grp,cex=.8,line=0)
  
  axis(1)
  #axis(2, at=c(0.5,.75,1,1.25,1.5,1.75,2),las=1)
  axis(2,at=c(0.8, 1, 1.25, 1.5,   2), las=2) 

  add.histogram(data.i$tmean) 
  
  abline(v=mintemp.i,lty=3) 
  abline(v=vlines.i,lty=2) 

}
 
################################################################################
#  
plotcities.panelage = function(predlist=list(PREDICTED.ALLAGES, PREDICTED.65PLUS, PREDICTED.50TO64, PREDICTED.0TO49), 
                               bluplist=list(MV.ALLAGES$BLUP1, MV.65PLUS$BLUP1, MV.50TO64$BLUP1, MV.0TO49$BLUP1), 
                               agenames=c("All ages", "Age 65+", "Ages 50-64", "Ages <50"), 
                               one.per.page="Y",subset.cities="allCities",subgroup.cities=cities.to.run,other.label=NULL, ...){
  
  col.in.plots=length(predlist)
  
  xlab <- expression(paste("Temperature (",degree,"C)"))
  if( one.per.page !="Y" ){
    pdf(paste(plotsfolder,scenario.running,"Temp_mortality_associations_panelage_",subset.cities,".pdf",sep=""),width=8.5,height=11)
    par(mfrow=c(4,col.in.plots))
  } else {
    pdf(paste(plotsfolder,scenario.running,"Temp_mortality_associations_panelage_",subset.cities,".pdf",sep=""),width=8.5,height=3)
    par(mfrow=c(1,col.in.plots))
  }
  
  for(i in subgroup.cities  ) {
    data <- dailytemp[[i]]
    these.knots =as.numeric(KNOTS1[i,]); this.bound = BOUNDS1[i,]
    argvar <- list(x=data$tmean,fun=varfun,knots=these.knots, Boundary.knots=this.bound)
    bvar <- do.call(onebasis,argvar)
    for(j in 1:length(predlist)){
      pred <- crosspred(bvar,coef=bluplist[[j]][[i]]$blup,vcov=bluplist[[j]][[i]]$vcov,
                        model.link="log",by=0.1,cen=predlist[[j]]$MMT$MMT1[i])
      plot.city.oneage(pred.i=pred, data.i=dailytemp[[i]], mintemp.i=predlist[[j]]$MMT$MMT1[i],vlines.i=c(metadata2$p5[i], metadata2$p95[i] ),age.grp=agenames[j])
      }
    title(paste(metadata2$City_link[i],", ",metadata2$country.name[i] ,sep=""),outer=TRUE,line=-1.2, cex=1.2)
    } 
  dev.off()
} 
