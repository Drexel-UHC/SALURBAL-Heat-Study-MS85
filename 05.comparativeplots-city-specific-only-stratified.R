#create estimated temperature-mortality associations for each city
 
PredictedMV1 = list()
PredictedRaw = list()

dir.create(plotsfolder.age)
################################################################################
# PLOTS
################################################################################
dir.create(paste(plotsfolder.age,"/AllCities",sep=""))

print("Creating comparative plots for different meta-analysis methods")
pdf(file=paste(plotsfolder.age,"AllCities/CompareMethods_allcities.pdf",sep=""))

for(i in seq(length(datasetlist))) {
  #cat(i,"")
  
  data <- dailytemp[[i]]  #daily temperature data for city i
  
  these.knots = KNOTS1[i,] ; this.bound = BOUNDS1[i,]
  argvar <- list(x=data$tmean,fun=varfun,#degree=vardegree,
                 knots=these.knots, Boundary.knots=this.bound )
  bvar <- do.call(onebasis,argvar)
  
  #plot prediction using city-specific knots (no smoothing)
  pred <- crosspred(bvar,coef=RED[[i]]$coefficients,vcov=RED[[i]]$vcov,
                    model.link="log",by=0.1,cen=mean(data$tmean,na.rm=T))
 # lines(pred$predvar,pred$allRRfit,col=2,lwd=1.5)
  PredictedRaw[[i]]=pred
  plot(PredictedRaw[[i]],xlab=expression(paste("Temperature (",degree,"C)")), 
       xlim=as.numeric(BOUNDS1[i,]),  ylim=c(0,2.5),ylab="RR",
       main=paste(cities$city[i], "\n Total deaths=", metadata$totaldeaths[i] )
  ) #could add city name here
  
  #add prediction using meta-analysis prediction using undjusted overall MV-meta, MV1
  pred <- crosspred(bvar,coef=blup1[[i]]$blup,vcov=blup1[[i]]$vcov,
                    model.link="log",by=0.1,cen=mean(data$tmean,na.rm=T))
  lines(pred$predvar,pred$allRRfit,col=2,lwd=1.5)
  PredictedMV1[[i]]=pred
  
 
  add.histogram(data$tmean)

  #title(cities$cityname[i])
 
} 
dev.off()
