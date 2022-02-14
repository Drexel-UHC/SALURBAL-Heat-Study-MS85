
outnames=c(paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_est", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_low", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_high", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_se", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_est", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_low", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_high", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_se", sep=""))


###############################################
#                                             #
# ATTR / AF -  ALL AGES -- FULLY MODEL BASED  #
#                                             #
###############################################
print("Running all ages - attR from single meta - SMOOTHED CURVE")
# Using single Meta              #####
######################################
blup=MV.ALLAGES$BLUP1
mintempcity=PREDICTED.ALLAGES$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=FALSE
this.strata=NULL

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
ALLAGES_MV1.attrf.attrn = data.frame(age="all",method="SM",curve="smooth",out.attrf.attrn )
ALLAGES_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
ALLAGES_MV1.summary = data.frame(age="all",method="SM",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)


###############################################
#                                             #
# ATTR / AF -  65PLUS -- FULLY MODEL BASED    #
#                                             #
###############################################
print("Running 65+ - attR from single meta - SMOOTHED CURVE")

# Using single Meta              #####
######################################
blup=MV.65PLUS$BLUP1
mintempcity=PREDICTED.65PLUS$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage=='65+'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
AGE_65PLUS_MV1.attrf.attrn = data.frame(age="65+",method="SM",curve="smooth",out.attrf.attrn )
AGE_65PLUS_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
AGE_65PLUS_MV1.summary = data.frame(age="65+",method="SM",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)




###############################################
#                                             #
# ATTR / AF -  LESS 65 -- FULLY MODEL BASED  #
#                                             #
###############################################
print("Running <65 - attR from single meta - SMOOTHED CURVE")

# Using single Meta              #####
######################################
blup=MV.LESS65$BLUP1
mintempcity=PREDICTED.LESS65$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage!='65+'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
LESS65_MV1.attrf.attrn = data.frame(age="<65",method="SM",curve="smooth",out.attrf.attrn )
LESS65_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
LESS65_MV1.summary = data.frame(age="<65",method="SM",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)



#####################################################################
#####################################################################
#####################################################################

write.csv( rbind(
	ALLAGES_MV1.summary,
	AGE_65PLUS_MV1.summary, 
	LESS65_MV1.summary), file=paste( results.tables.path , scenario.running,"_AttFrac-AttN-Summaries_SingleMeta.csv" , sep="") )

write.csv( rbind(
	ALLAGES_MV1.attrf.attrn, 
	AGE_65PLUS_MV1.attrf.attrn , 
	LESS65_MV1.attrf.attrn), file=paste( results.tables.path , scenario.running, "_AttFrac-AttN-City-Specific-SmoothedForDisplay_SingleMeta.csv" , sep="") )


 
#####################################################################
#####################################################################
#####################################################################

