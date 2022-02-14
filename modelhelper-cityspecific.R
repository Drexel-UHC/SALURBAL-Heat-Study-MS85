# DEFINE THE CROSSBASIS
#argvar <- list(fun=varfun,knots=these.knots, degree=vardegree, Boundary.knots=this.bound)
argvar <- list(fun=varfun,knots=these.knots, Boundary.knots=this.bound)
cb <- crossbasis(data$tmean,lag=lag,argvar=argvar,
                 arglag=list(knots=logknots(lag,lagnk)), group=data$group.series)
#summary(cb)

# NB: NO CENTERING NEEDED HERE, AS THIS DOES NOT AFFECT COEF-VCOV
if(useoffset=="Y"){
  model <- glm(formula,data, offset=logpop, family=quasipoisson,na.action="na.exclude", maxit=50)
  } else {
  model <- glm(formula,data, family=quasipoisson,na.action="na.exclude")
  }
cen <- mean(data$tmean,na.rm=T)
mod.fit <- fqaic(model)
 
# REDUCTION TO OVERALL CUMULATIVE
red <- crossreduce(cb,model,cen=cen)

 