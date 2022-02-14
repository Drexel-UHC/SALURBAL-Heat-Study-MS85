# this file reads in / produces
# (a) "metadata" file that includes 1 row per city, for the cities included in the analysis
# (b) a list of datasets for each city, which contains the daily mortality counts, daily temperature, etc. which are used for city-specific models

# METADATA FOR LOCATIONS

metadata0 = read.sas7bdat(paste(metadatapath,"city_level_temp.sas7bdat",sep=""))  
hasmortality = read.sas7bdat(paste(metadatapath,"hasmortality_indic.sas7bdat",sep="")) #indicator to identify if SALURBAL City has mortality data
clusters=read.csv(paste("//files.drexel.edu/encrypted/SOPH/UHC/Projects/SALURBAL HEAT Study/Analysis/Clusters/Results/ECDFClusters.csv")) 
     #temperature cluster indicators
climate.labels=read.csv(paste(climatedatapath, "climate_labels2.csv",sep="")) #climate zone labels
inAICcalculations=read.csv(paste(metadatapath,"inAICcalculations.csv",sep=""))
inAICcalculations=subset(inAICcalculations, select=c(nsalid1,inAIC))

#merge mortality indicator
metadata = merge(metadata0,hasmortality, by=c("nsalid1"))
metadata = subset(metadata, hasMortality==1)

#merge AIC indicator
metadata = merge(metadata,inAICcalculations, by=c("nsalid1"))
metadata = subset(metadata, inAIC==1)

#merge climate data (used for plotting later)
metadata = merge(metadata, climate.labels, by=c("KP_climate_zone"), all.x=TRUE, all.y=FALSE)
metadata$KP_Precip_num = as.numeric(metadata$KP_Precipitation)
metadata$KP_Precip_num[metadata$KP_Precip_Interpret  == "Tundra"] = 5

#merge cluster assignments
metadata = merge(metadata,clusters, by=c("nsalid1"))

reorder=order(metadata$nsalid1)
metadata=metadata[reorder,]
  
#variable with file names:
metadata$filename = paste("c",metadata$nsalid1,sep="")

#list of dataset names
datasetlist=paste(citydatapath,metadata$filename,".sas7bdat",sep="")
   # foo=dir(citydatapath)

# data table with city id's and city "names"
# nsalid1 is a placeholder for now for these two variables. When we create plots, these would be actual city names
metadata$city=metadata$nsalid1
metadata$cityname=metadata$nsalid1
cities <- data.frame(nsalid1=metadata$nsalid1, city=metadata$city, cityname=metadata$cityname)

#counter for cities to run
cities.to.run = 1:dim(metadata)[1]
#cities.to.run = 1:2  #for when we want to test things

###
###   CHECK: is the dimension of metadata the same as the length of dataset list?
###   if not, there is something wrong!!

dim(metadata)
length(datasetlist)

################################################################################################################
# READ city psecific data, store in list
# Also obtain summaries
#     count total deaths and average population in city
#     these are used in ATTR calculations later
################################################################################################################
dailytemp=list()
DATASET.LIST=list()
DATASET.LIST.FINEGRAINAGE = list()
deaths.by.age=list()
metadata$totaldeaths= NA
metadata$AvPop.in1000s= NA
metadata$years.of.data = NA
metadata$AvPop.in1000 = NA
per.temp = data.frame(matrix(NA,ncol=13,nrow=length(cities.to.run)))
names(per.temp)=c("p0", "p1", "p2_5", "p5", "p10", "p25", "p50", "p75", "p90", "p95", "p97_5", "p99", "p100")

for(i in seq(length(datasetlist))) {
  cat(i,"")

  junkdat = read.my.data(datasetlist[i])
  junkdat$maletxt = as.factor(junkdat$male)
  junkdat$group.series=as.factor(junkdat$thisage:junkdat$maletxt)
  DATASET.LIST.FINEGRAINAGE[[i]]=junkdat
  rm(junkdat)
  
  data=DATASET.LIST.FINEGRAINAGE[[i]]
  dailytemp[[i]] = unique( subset(data, select=c("nsalid1","tmean","date")  )  )

  metadata$totaldeaths[i] = sum(data$death)
  metadata$years.of.data[i] = length(unique(data$year))
  metadata$AvPop.in1000s[i] = sum(exp(data$logpop))/(365.25*metadata$years.of.data[i])

  deathsbyage=aggregate(cbind(death,Pop.in1000s=exp(logpop)/(365.25*length(unique(data$year))))~thisage , data=data, FUN = sum )
  deathsbyage$mortality.per1000=(deathsbyage$death/metadata$years.of.data[i])/deathsbyage$Pop.in1000s
  deaths.by.age[[i]] =deathsbyage

  per.temp[i,]=quantile(data$tmean, prob=c(0, 0.01, 0.025, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 0.975, 0.99, 1))
}
metadata$mortality.per1000 = (metadata$totaldeaths/metadata$years.of.data) / metadata$AvPop.in1000s
per.temp$nsalid1 = metadata$nsalid1

metadata=merge(metadata,per.temp, by=c("nsalid1")  )
 

metadata$rangelow=metadata$p50-metadata$p1
metadata$rangehigh=metadata$p99-metadata$p50

metadata$country.num=floor(metadata$nsalid1/1000)
table(metadata$country.num)
#AR  BR   CL  PE  CR  SV  GT  MX  PA
#33  152  21  23   1   3   3  92   3
metadata$country.group="    "
grp.cities=which(metadata$country.num==201 | metadata$country.num==202 | metadata$country.num==203 | metadata$country.num==206)
metadata$country.group[grp.cities] = "CenAm"
metadata$country.group[(metadata$country.num==101)] = "Argentina"
metadata$country.group[(metadata$country.num==102)] = "Brazil"
metadata$country.group[(metadata$country.num==204)] = "Mexico"
metadata$country.group[(metadata$country.num==103)] = "Chile"
metadata$country.group[(metadata$country.num==105)] = "Peru"


latlong=read.csv("//files.drexel.edu/encrypted/SOPH/UHC/Projects/SALURBAL HEAT Study/Data/Files/Climate Zone/Processed Data/SALURBAL_L1_latlong_elev_BECCityCenter.csv")
latlong=latlong[order(latlong$SALID1),]
metadata2=merge(metadata,latlong,by=c("SALID1"))
metadata2=metadata2[order(metadata2$SALID1),]


#create variable that combines two of the climate zone variables into a single factor variable
metadata$KP_level1_precip = paste(metadata$KP_level1_Interpretation, metadata$KP_Precip_num)

# EXTRACT PERCENTILES from the meta data file, these are for calcualtions of excess mortality:
per <- subset(metadata, select=c(p2_5,p10,p25,p50,p75,p90,p97_5))
 
#this defines a data table with the values of the city-specific knots
KNOTS1 = as.matrix(subset(metadata, select=return(eval(parse(text=specify.percentile.knots))) ))  
BOUNDS1 = as.matrix(subset(metadata, select=c(p0,p100)))    #  THIS STAYS THE SAME
BOUNDS1[,1]= BOUNDS1[,1] - 0.001
BOUNDS1[,2]= BOUNDS1[,2] + 0.001


if(collapse.under50=="Y"){
  for(i in 1:length(DATASET.LIST.FINEGRAINAGE)){
    this.data=DATASET.LIST.FINEGRAINAGE[[i]]
    this.data$thisage.finegrain=this.data$thisage
    rows.to.collapse = which( !(this.data$thisage.finegrain == "65+" | this.data$thisage.finegrain =="50-64" ) )
    levels(this.data$thisage)=c(levels(this.data$thisage),"0-49")
    this.data$thisage[rows.to.collapse]="0-49"
    this.data$temp.popcount=exp(this.data$logpop)
    foo=aggregate(cbind(death,temp.popcount)~ nsalid1+tmean+dow+date+year+thisage , data=this.data , FUN=sum)
    foo$logpop=log(foo$temp.popcount)
    foo$male = "bothsex"
    foo1=subset(foo, select=varkeep)
    foo1$thisage=droplevels(foo1$thisage)
    foo1$group.series=as.factor(paste(foo1$thisage,foo1$male, sep=""))
    DATASET.LIST[[i]]=foo1
    rm(foo1)
    rm(this.data)
  }
}
 
if(collapse.under50=="N"){
  DATASET.LIST=DATASET.LIST.FINEGRAINAGE
  for(i in 1:length(DATASET.LIST.FINEGRAINAGE)){
    this.data=DATASET.LIST.FINEGRAINAGE[[i]]
    this.data$thisage.sex=paste(this.data$male,this.data$thisage,sep="-")
    this.data$thisage=as.factor(this.data$thisage.sex)
    DATASET.LIST[[i]]=this.data
    rm(this.data)
  }
}
