################################################################################
# Updated version of the code for the analysis in:
#
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#   http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: 15 January 2017
# * an updated version of this code, compatible with future versions of the
#   software, is available at:
#   https://github.com/gasparrini/2015_gasparrini_Lancet_Rcodedata
################################################################################

################################################################################
# COMPUTE THE ATTRIBUTABLE DEATHS FOR EACH CITY, WITH EMPIRICAL CI
# ESTIMATED USING THE RE-CENTERED BASES
################################################################################

# LOAD THE FUNCTION FOR COMPUTING THE ATTRIBUTABLE RISK MEASURES
source(paste(codepath,"attrdl.strat.R", sep=""))

# CREATE THE VECTORS TO STORE THE TOTAL MORTALITY (ACCOUNTING FOR MISSING)
totdeath <- rep(NA,nrow(cities))
names(totdeath) <- cities$city

# CREATE THE MATRIX TO STORE THE ATTRIBUTABLE DEATHS 
 
matsim <- array(NA,dim=c(nrow(cities),7,2),dimnames=list(cities$city,
                                                           c("glob","cold","heat","excold","exheat","modcold","modheat"),c("an","af")))

# CREATE THE ARRAY TO STORE simulation results
af.arraysim <- array(NA,dim=c(nrow(cities),7,nsim),dimnames=list(cities$city,
                    c("glob","cold","heat","excold","exheat","modcold","modheat")))
an.arraysim <- array(NA,dim=c(nrow(cities),7,nsim),dimnames=list(cities$city,
                                                                 c("glob","cold","heat","excold","exheat","modcold","modheat")))
city.sim <- array(NA,dim=c(nrow(cities),7,8),dimnames=list(cities$city,
                                                              c("glob","cold","heat","excold","exheat","modcold","modheat"),
                        c("an.sim.mean" , "an.sim.se" , "an.low" , "an.high" ,"af.sim.mean" , "af.sim.se" , "af.low" , "af.high" )))

################################################################################

# RUN THE LOOP
for(i in seq(datasetlist)){
  
  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  #data <- read.my.data(datasetlist[i], subset=to.subset, strata=this.strata) 
  if(to.subset==TRUE){ 
	data = subset(DATASET.LIST[[i]] , return(eval(parse(text=this.strata)))) 
 	data$thisage=droplevels(data$thisage)
	data$group.series=droplevels(data$group.series)
    } else {
    data=DATASET.LIST[[i]]
  }
    data = data[order(data$group.series, data$date),]

  # DERIVE THE CROSS-BASIS
  # NB: CENTERING POINT DIFFERENT THAN ORIGINAL CHOICE OF 75TH
  these.knots=as.numeric(THESE.KNOTS[i,])
  this.bound=as.numeric(THESE.BOUNDS[i,])  
  argvar <- list(x=data$tmean,fun=varfun,
                 knots=these.knots,#degree=vardegree, 
                 Bound=this.bound)
  cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
    arglag=list(knots=logknots(lag,lagnk)), group=data$group.series)
  
  
  # COMPUTE THE ATTRIBUTABLE DEATHS
  # NB: THE REDUCED COEFFICIENTS ARE USED HERE
  matsim[i,"glob",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"cold",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                range=c(-100,mintempcity[i]),strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"heat",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                range=c(mintempcity[i],100),strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"excold",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                 vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                 range=c(-100,coldlimit[i]),strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"exheat",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                 vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                 range=c(heatlimit[i],100),strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"modcold",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                 vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                 range=c(coldlimit[i],mintempcity[i]),strat.by=data.frame(age=data$thisage,male=data$male))
  matsim[i,"modheat",]=attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                 vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                 range=c(mintempcity[i],heatlimit[i]),strat.by=data.frame(age=data$thisage,male=data$male))
  
  
 # COMPUTE EMPIRICAL OCCURRENCES OF THE ATTRIBUTABLE DEATHS
 # USED TO DERIVE CONFIDENCE INTERVALS
 # nsim=10
  #hot and cold
  temp= attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                      vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                      sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"glob",] <- temp$city.summary
  af.arraysim[i,"glob",] <- temp$afsim
  an.arraysim[i,"glob",] <- temp$ansim

  # heat
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                      vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                      range=c(mintempcity[i],100),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"heat",] <- temp$city.summary
  af.arraysim[i,"heat",] <- temp$afsim
  an.arraysim[i,"heat",] <- temp$ansim
  
  #cold
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                                      vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                                      range=c(-100,mintempcity[i]),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"cold",] <- temp$city.summary
  af.arraysim[i,"cold",] <- temp$afsim
  an.arraysim[i,"cold",] <- temp$ansim

  # extreme heat
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                       vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                       range=c(heatlimit[i],100),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"exheat",] <- temp$city.summary
  af.arraysim[i,"exheat",] <- temp$afsim
  an.arraysim[i,"exheat",] <- temp$ansim
  
  #extreme cold
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                       vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                       range=c(-100,coldlimit[i]),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"excold",] <- temp$city.summary
  af.arraysim[i,"excold",] <- temp$afsim
  an.arraysim[i,"excold",] <- temp$ansim
  

  # mod heat
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                       vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                       range=c(mintempcity[i],heatlimit[i]),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"modheat",] <- temp$city.summary
  af.arraysim[i,"modheat",] <- temp$afsim
  an.arraysim[i,"modheat",] <- temp$ansim
  
  #mod cold
  temp <- attrdl.strat(data$tmean,cb,data$death,coef=blup[[i]]$blup,
                       vcov=blup[[i]]$vcov,type="an",dir="forw",cen=mintempcity[i],
                       range=c(coldlimit[i],mintempcity[i]),sim=T,nsim=nsim,strat.by=data.frame(age=data$thisage,male=data$male))
  city.sim[i,"modcold",] <- temp$city.summary
  af.arraysim[i,"modcold",] <- temp$afsim
  an.arraysim[i,"modcold",] <- temp$ansim

  # STORE THE DENOMINATOR OF ATTRIBUTABLE DEATHS, I.E. TOTAL OBSERVED MORTALITY
  # CORRECT DENOMINATOR TO COMPUTE THE ATTRIBUTABLE FRACTION LATER, AS IN attrdl
  totdeath[i] <- sum(data$death,na.rm=T)
}

################################################################################
# CITY-SPECIFIC
################################################################################

# ATTRIBUTABLE NUMBERS
ancity <- matsim[,,"an"]
ancitylow <- apply(an.arraysim,c(1,2),quantile,0.025)
ancityhigh <- apply(an.arraysim,c(1,2),quantile,0.975)
ancity.se <- sqrt(apply(an.arraysim,c(1,2),var))

# ATTRIBUTABLE FRACTIONS
afcity <- 100*matsim[,,"af"]
afcitylow <- 100*apply(af.arraysim,c(1,2),quantile,0.025)
afcityhigh <- 100*apply(af.arraysim,c(1,2),quantile,0.975)
afcity.se <- 100*sqrt(apply(af.arraysim,c(1,2),var))

########################
# output stuff
########################
out.attrf.attrn = data.frame(cbind(mintempcity,totdeath, afcity, afcitylow, afcityhigh, afcity.se, ancity, ancitylow, ancityhigh, ancity.se))
names(out.attrf.attrn ) = c("mintempcity","totdeath",outnames)


