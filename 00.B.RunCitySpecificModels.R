# MODEL FORMULA
# The temperature variable is eventually fed into "cb" in the model formula.
# The temperature variable MUST be called "tmean", else, this can be re-named everytime the data are read in
#
# The count of deaths MUST be called "death", else, this can be re-named everytime the data are read in
#
# Other variables in the formula are:
#    death=counts of death
#    dow = day of week (e.g., 1-7)
#    date = numeric variable, it can simply be consecutive 1:ndays in city
#    year = literally calendar year for row of the dataset, e.g. 1998 or some other value that differentiates between years

if( get0("model.type", ifnotfound = "conventional")=="conventional"){
  print("Running conventional models")
  source(paste(codepath,"01.firststage-cityspecific-age-stratified.R",sep=""))
  write.csv(fitmat, file= AICfile)
  } else if( get0("model.type", ifnotfound = "junk") =="conditional"){
    print("Running CONDITIONAL models")
    source(paste(codepath,"01.firststage-cityspecific-age-stratified-conditional.R",sep=""))
    write.csv(fitmat, file= AICfile)  # this is deviance, not AIC
  }

RESULTS.ALLAGES = list(COEF=coef1, VCOV=vcov1, RED=red.1)
RESULTS.65PLUS  = list(COEF=coef2, VCOV=vcov2, RED=red.2)
RESULTS.LESS65  = list(COEF=coef3, VCOV=vcov3, RED=red.3)
RESULTS.50TO64  = list(COEF=coef4, VCOV=vcov4, RED=red.4)
RESULTS.0TO49   = list(COEF=coef5, VCOV=vcov5, RED=red.5)
save.image( file=outputfilename)

 
