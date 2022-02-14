################################################################################
# Adapted version of the code for the analysis in:
#   "Mortality risk attributable to high and low ambient temperature:
#     a multi-country study"
#   Antonio Gasparrini and collaborators
#   The Lancet - 2015
#   http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: 3 Oct 2020
# * an updated version of this code, to be run with SALURBAL data
################################################################################

################################################################################
# FIRST-STAGE ANALYSIS: RUN THE MODEL IN EACH CITY, REDUCE AND SAVE
################################################################################
nmodels=5
# model 1 = all ages
# model 2 = 65+
# model 3 = <65
# model 4 = 50-64 
# model 5 = <50

################################################################################
# CREATE THE OBJECTS TO STORE THE RESULTS

# COEFFICIENTS AND VCOV FOR OVERALL CUMULATIVE SUMMARY FOR EACH MODEL
# model 1 
coef1 <- matrix(NA,nrow(metadata),dim(KNOTS1)[2]+vardegree-2,dimnames=list(metadata$city))
vcov1 <- vector("list",nrow(metadata)) ; names(vcov1) <- metadata$city ; red.1 <- vector("list",nrow(metadata))
# model 2 
coef2 <- matrix(NA,nrow(metadata),dim(KNOTS1)[2]+vardegree-2,dimnames=list(metadata$city))
vcov2 <- vector("list",nrow(metadata)) ; names(vcov2) <- metadata$city ; red.2 <- vector("list",nrow(metadata))
#Model 3
coef3 <- matrix(NA,nrow(metadata),dim(KNOTS1)[2]+vardegree-2,dimnames=list(metadata$city))
vcov3 <- vector("list",nrow(metadata)) ; names(vcov3) <- metadata$city ; red.3 <- vector("list",nrow(metadata))
#Model 4
coef4 <- matrix(NA,nrow(metadata),dim(KNOTS1)[2]+vardegree-2,dimnames=list(metadata$city))
vcov4 <- vector("list",nrow(metadata)) ; names(vcov4) <- metadata$city ; red.4 <- vector("list",nrow(metadata))
#Model 5
coef5 <- matrix(NA,nrow(metadata),dim(KNOTS1)[2]+vardegree-2,dimnames=list(metadata$city))
vcov5 <- vector("list",nrow(metadata)) ; names(vcov5) <- metadata$city ; red.5 <- vector("list",nrow(metadata))

fitmat<- matrix(NA,nrow(metadata), nmodels)


################################################################################
# RUN THE LOOP

# LOOP
time <- proc.time()[3]
for(i in cities.to.run ) {

  # PRINT
  cat(i,"")
  
  # EXTRACT THE DATA
  # CHANGE THIS TO READ IN THE DATASET WE WANT. 
  data1.foo <- DATASET.LIST[[i]]
    data1 = data1.foo[ order(data1.foo$group.series, data1.foo$date),]
    rm(data1.foo)
  
    data1$thisage=droplevels(data1$thisage)
    data1$month  <- as.factor(months(data1$date))
    data1$year   <- as.factor(format(data1$date, format="%Y") )
    data1$dow    <- as.factor(weekdays(data1$date))
    data1$maletxt = as.factor(data1$male) 
    data1$stratum <- as.factor(data1$year:data1$month:data1$dow:data1$thisage:data1$maletxt)
    #data1$stratum <- as.factor(data1$year:data1$month:data1$dow:data1$maletxt)
    
    
  these.knots =as.numeric(KNOTS1[i,]); this.bound = BOUNDS1[i,]
  
  #formula <- death~cb+thisage
  formula <- death~cb 
  
  # MODEL 1, All ages
  data=data1
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
            error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in SALID=",metadata$nsalid1[i], "Allages"))})
  model.all=model
  m1 = list(red=red,fit=mod.fit); 
  coef1[i,] <- coef(m1$red) ; vcov1[[i]] <- vcov(m1$red)
  fitmat[i,1] <- m1$fit     ; red.1[[i]] <- m1$red


  # MODEL 2, Age 65+
  data=subset(data1, thisage == '65+')
	data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
            error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in SALID=",metadata$nsalid1[i], "65+"))})
  model.65plus=model
  m2 = list(red=red,fit=mod.fit); 
  coef2[i,] <- coef(m2$red) ; vcov2[[i]] <- vcov(m2$red)
  fitmat[i,2] <- m2$fit     ; red.2[[i]] <- m2$red

  # MODEL 3, Age < 65
  data=subset(data1, thisage != '65+')
	data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
            error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in SALID=",metadata$nsalid1[i], "<65"))})
  model.less65=model
  m3 = list(red=red,fit=mod.fit); 
  coef3[i,] <- coef(m3$red) ; vcov3[[i]] <- vcov(m3$red)
  fitmat[i,3] <- m3$fit     ; red.3[[i]] <- m3$red

  # MODEL 4, Age 50-64
  data=subset(data1, thisage == '50-64')
	data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
            error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in SALID=",metadata$nsalid1[i], "50-64"))})
  model.50to64=model
  m4 = list(red=red,fit=mod.fit); 
  coef4[i,] <- coef(m4$red) ; vcov4[[i]] <- vcov(m4$red)
  fitmat[i,4] <- m4$fit     ; red.4[[i]] <- m4$red

  # MODEL 5, Age <50 
  data=subset(data1, thisage == '0-49')
	data$group.series=droplevels(data$group.series)
  tryCatch( source(paste(codepath,"modelhelper-cityspecific-conditional.R",sep="")), 
            error=function(e){mod.fit<<- NA; model <<- NULL; print(paste("error in SALID=",metadata$nsalid1[i], "0-49"))})
  model.0to49=model
  m5 = list(red=red,fit=mod.fit); 
  coef5[i,] <- coef(m5$red) ; vcov5[[i]] <- vcov(m5$red)
  fitmat[i,5] <- m5$fit     ; red.5[[i]] <- m5$red
  
  
#Save the entire model for the future - will be needed to construct lags later
if(saveallraw=="Y"){ 
  save(data1,model.all,model.65plus,model.less65,model.50to64,model.0to49,m1,m2,m3,m4,m5,varfun,these.knots,this.bound,lag,lagnk, 
       file=paste(rawoutput,"City_",cities$city[i],".Rdata", sep="") )
  }

rm(model.all,model.65plus,model.less65,model.50to64,model.0to49,m1,m2,m3,m4,m5)
 
} 

save.image( file=outputfilename)

