
outnames=c(paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_est", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_low", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_high", sep=""),
           paste("attr_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_se", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_est", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_low", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_high", sep=""),
           paste("attN_",c("glob", "cold", "heat", "excold", "exheat", "modcold", "modheat"), "_se", sep=""))

nsim = 1000   
#nsim=3
THESE.KNOTS = KNOTSFORCITY 
THESE.BOUNDS = BOUNDSFORCITY
extreme.temp=data.frame(cold=metadata$p5,heat=metadata$p95) #this defines that 'extreme' temperatures are at p5 and p95

###############################################
#                                             #
# ATTR / AF -  ALL AGES -- FULLY MODEL BASED  #
#                                             #
###############################################
print("Running all ages - attR from cluster")
blup=MV.ALLAGES$BLUP1
mintempcity=MV.ALLAGES$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=FALSE
this.strata=NULL

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
ALLAGES_MV1.attrf.attrn = data.frame(age="all",method="CL",curve="cluster",out.attrf.attrn )
ALLAGES_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
ALLAGES_MV1.summary = data.frame(age="all",method="CL",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)

 

###############################################
#                                             #
# ATTR / AF -  65PLUS -- FULLY MODEL BASED    #
#                                             #
###############################################
print("Running 65+ - attR from cluster")

# Using single Meta              #####
######################################
blup=MV.65PLUS$BLUP1
mintempcity=MV.65PLUS$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage=='65+'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
AGE_65PLUS_MV1.attrf.attrn = data.frame(age="65+",method="CL",curve="cluster",out.attrf.attrn )
AGE_65PLUS_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
AGE_65PLUS_MV1.summary = data.frame(age="65+",method="CL",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)

 
###############################################
#                                             #
# ATTR / AF -  LESS 65 -- FULLY MODEL BASED  #
#                                             #
###############################################
print("Running <65 - attR from cluster")

# Using single Meta              #####
######################################
blup=MV.LESS65$BLUP1
mintempcity=MV.LESS65$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage!='65+'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
LESS65_MV1.attrf.attrn = data.frame(age="<65",method="CL",curve="cluster",out.attrf.attrn )
LESS65_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
LESS65_MV1.summary = data.frame(age="<65",method="CL",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)

 

###############################################
#                                             #
# ATTR / AF -  50TO64 -- FULLY MODEL BASED    #
#                                             #
###############################################
print("Running 50-64 - attR from cluster")

# Using single Meta              #####
######################################
blup=MV.50TO64$BLUP1
mintempcity=MV.50TO64$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage=='50-64'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
AGE_50TO64_MV1.attrf.attrn = data.frame(age="50-64",method="CL",curve="cluster",out.attrf.attrn )
AGE_50TO64_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
AGE_50TO64_MV1.summary = data.frame(age="50-64",method="CL",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)
 
###############################################
#                                             #
# ATTR / AF -  0TO49 -- FULLY MODEL BASED    #
#                                             #
###############################################
print("Running 0TO49 - attR from cluster")

# Using single Meta              #####
######################################
blup=MV.0TO49$BLUP1
mintempcity=MV.0TO49$MMT$MMT1
heatlimit=apply(cbind(extreme.temp$heat,mintempcity),1,max,na.rm=T)
coldlimit=apply(cbind(extreme.temp$cold,mintempcity),1,min,na.rm=T) 
to.subset=TRUE 
this.strata="thisage=='0-49'"

source(paste(codepath,"03.attr-mod-all-cold-hot.R",sep=""))  #creates out.attrf.attrn 
AGE_0TO49_MV1.attrf.attrn = data.frame(age="0-49",method="CL",curve="cluster",out.attrf.attrn )
AGE_0TO49_MV1.sims = list( city=city.sim, af=af.arraysim, an=an.arraysim )

source(paste(codepath,"summary-attr.R",sep=""))  
AGE_0TO49_MV1.summary = data.frame(age="0-49",method="CL",rbind(overall.summary,country.summary,KPLevel1.summary,KPLevel1.plus.summary,KPzone.summary,Clusters6.summary,Clusters12.summary))

rm(list=c("out.attrf.attrn","blup","mintempcity"))
save.image( file=outputfilename)

 

#####################################################################
#####################################################################
#####################################################################

write.csv( rbind(
	ALLAGES_MV1.summary,
	AGE_65PLUS_MV1.summary, 
	LESS65_MV1.summary,
        AGE_50TO64_MV1.summary,
        AGE_0TO49_MV1.summary ), file=paste( results.tables.path , scenario.running,"_AttFrac-AttN-Summaries_fromCluster.csv" , sep="") )

write.csv( rbind(
	ALLAGES_MV1.attrf.attrn, 
	AGE_65PLUS_MV1.attrf.attrn , 
	LESS65_MV1.attrf.attrn,
	AGE_50TO64_MV1.attrf.attrn,
	AGE_0TO49_MV1.attrf.attrn), file=paste( results.tables.path , scenario.running, "_AttFrac-AttN-City-Specific-SmoothedForDisplay_fromCluster.csv" , sep="") )
 

 
#####################################################################
#####################################################################
#####################################################################

