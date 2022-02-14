
# META-ANALYSIS - overall - adjusted for various charactersitics
# ################################################################################

mv1  <- mvmeta(COEF ~   ns(metadata$p50,df=2) +  ns(metadata$rangehigh,df=2) +  ns(metadata$rangelow,df=2)  + 
                           as.factor(metadata$KP_level1_Interpretation ), 
                         VCOV, data=cities, control=list(showiter=F))
# OBTAIN BLUPS
blup1 <- blup(mv1,vcov=T) 

 