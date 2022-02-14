#run the actual meta analysis, for each age group:
##############################################
print("Metanalysis for All ages")

#ALL AGES
COEF=RESULTS.ALLAGES$COEF
VCOV=RESULTS.ALLAGES$VCOV
RED = RESULTS.ALLAGES$RED
source(paste(codepath,"02.secondstage-city-specificonly-stratified.R",sep=""))
MV.ALLAGES =list(MV1=mv1, BLUP1=blup1)
plotsfolder.age= paste(plotsfolder,"ALLAGES/",sep="")
source(paste(codepath,"05.comparativeplots-city-specific-only-stratified.R",sep=""))
source(paste(codepath,"02.a-minimum-mortality.R",sep=""))
write.csv(MMT, file=paste(results.tables.path,"MMTALLAGES.csv", sep=""))
PREDICTED.ALLAGES = list(PredictedMV1=PredictedMV1 , PredictedRaw=PredictedRaw, 
                         RR.IN.PCT.1=RR.IN.PCT.1,  MMT=MMT)
rm(mv1, blup1, PredictedMV1 , PredictedRaw,  RR.IN.PCT.1)

save.image( file=outputfilename)

print("Metanalysis for 65+")

#65PLUS
COEF=RESULTS.65PLUS$COEF
VCOV=RESULTS.65PLUS$VCOV
RED =RESULTS.65PLUS$RED
source(paste(codepath,"02.secondstage-city-specificonly-stratified.R",sep=""))
MV.65PLUS =list(MV1=mv1, BLUP1=blup1 )
plotsfolder.age= paste(plotsfolder,"65PLUS/",sep="")
source(paste(codepath,"05.comparativeplots-city-specific-only-stratified.R",sep=""))
source(paste(codepath,"02.a-minimum-mortality.R",sep=""))
write.csv(MMT, file=paste(results.tables.path,"MMT65PLUS.csv", sep=""))
PREDICTED.65PLUS =  list(PredictedMV1=PredictedMV1 ,  PredictedRaw=PredictedRaw, 
                         RR.IN.PCT.1=RR.IN.PCT.1,  MMT=MMT)
rm(mv1, blup1, PredictedMV1 , PredictedRaw,  RR.IN.PCT.1)
save.image( file=outputfilename)

print("Metanalysis for <65")

#LESS65 
COEF=RESULTS.LESS65$COEF
VCOV=RESULTS.LESS65$VCOV
RED =RESULTS.LESS65$RED
source(paste(codepath,"02.secondstage-city-specificonly-stratified.R",sep=""))
MV.LESS65 =list(MV1=mv1, BLUP1=blup1, MV2=mv2, BLUP2=blup2, MV3=mv3, BLUP3=blup3, MV6=mv6, BLUP6=blup6)
plotsfolder.age= paste(plotsfolder,"LESS65/",sep="")
source(paste(codepath,"05.comparativeplots-city-specific-only-stratified.R",sep=""))
source(paste(codepath,"02.a-minimum-mortality.R",sep=""))
write.csv(MMT, file=paste(results.tables.path,"MMTLESS65.csv", sep=""))
PREDICTED.LESS65 =   list(PredictedMV1=PredictedMV1 ,  PredictedRaw=PredictedRaw, 
                         RR.IN.PCT.1=RR.IN.PCT.1,  MMT=MMT)
rm(mv1, blup1, PredictedMV1 , PredictedRaw,  RR.IN.PCT.1)

save.image( file=outputfilename)
