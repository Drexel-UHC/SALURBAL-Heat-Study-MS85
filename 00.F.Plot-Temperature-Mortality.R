xlab="Temperature"

#Read in names
latlong=read.csv(paste(climatedatapath,"SALURBAL_L1_latlong_elev_BECCityCenter.csv", sep=""))
metadata2=merge(metadata,latlong,by=c("SALID1"))
metadata2$country.name = metadata2$country.group
metadata2$country.name[which(metadata$country=="CR")] = "Costa Rica"
metadata2$country.name[which(metadata$country=="GT")] = "Guatemala"
metadata2$country.name[which(metadata$country=="PA")] = "Panama"
metadata2$country.name[which(metadata$country=="SV")] = "El Salvador" 
metadata2$range=metadata2$p99-metadata$p1 
metadata2$KPLevel1_Precip = paste(metadata2$KP_level1_Interpretation , metadata2$KP_Precip_Interpret, sep=", ") 

source(paste(codepath,"FinalPlotFunctions.R",sep=""))
 
plot.by.list( list( blup=MV.65PLUS$BLUP1, mintempcity=PREDICTED.65PLUS$MMT$MMT1,age.grp="65Plus") )
plot.by.list( list( blup=MV.LESS65$BLUP1, mintempcity=PREDICTED.LESS65$MMT$MMT1,age.grp="LESS65") )
plot.by.list( list( blup=MV.ALLAGES$BLUP1, mintempcity=PREDICTED.ALLAGES$MMT$MMT1,age.grp="ALLAGES") )

plot.by.list( list( blup=MV.50TO64$BLUP1, mintempcity=PREDICTED.50TO64$MMT$MMT1,age.grp="50TO64") )
plot.by.list( list( blup=MV.0TO49$BLUP1, mintempcity=PREDICTED.0TO49$MMT$MMT1,age.grp="0TO49") )

plotcities.panelage(one.per.page = "Y")