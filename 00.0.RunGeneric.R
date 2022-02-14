################################################################################
# Adapted version of the code for the analysis in:
#   "Mortality risk attributable to high and low ambient temperature: a multi-country study"
#   by: Antonio Gasparrini and collaborators
#   The Lancet - 2015; from: http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
################################################################################

################################################################################
##DECLARE FILE DIRECTORIES GENERAL  
################################################################################
overallpath = "//files.drexel.edu/encrypted/SOPH/UHC/Projects/SALURBAL HEAT Study/"
metadatapath = paste(overallpath,"Data/Files/Derived Data/Meta Data/",sep="")
climatedatapath = paste(overallpath,"Data/Files/Climate Zone/Processed Data/",sep="")
codepath = paste(overallpath,"Analysis/DLMAnalysis-AllAnalysis/MainDLMCode/",sep="")
deriveddatapath = paste(overallpath,"Data/Files/Derived Data/",sep="")  



#data path automated per above
citydatapath = paste(deriveddatapath,cause,"/",sep="")  #automatically create path to read in data
# knots for temperature
specify.percentile.knots = paste("c(",paste( input.knots, collapse = ","),")",sep="")          

print("USING DATA:")
print(citydatapath)

#create additional paths and directories based on the above 
####################################################################################
# Results
resultspath = paste(root.path.output,scenario.running,"/", sep="")
if( !dir.exists(resultspath)){dir.create(resultspath, recursive = TRUE)}

print("OUTPUT TO")
print(resultspath)
#R space that will contain the coefficients and vcov for all cities
outputfilename = paste(resultspath,scenario.running,".Rdata",sep="")

#folders where plots are saved
plotsfolder= paste(resultspath,"CitySpecificPlots/",sep="")
if( !dir.exists(plotsfolder)){dir.create(plotsfolder)}

results.tables.path = paste(resultspath,"/NumericOutput/", sep="")
if( !dir.exists(results.tables.path)){dir.create(results.tables.path)}

#file to save AIC results, if needed later
AICfile = paste(results.tables.path,scenario.running,"AIC.csv",sep="")

# will we save the full model object?  
saveallraw="Y"     # if we select "Y", then an .Rdata file gets created for each
# city, each containing the full results for the city 

rawoutput =paste(resultspath,"RawModelOutputs/",sep="")
if( !dir.exists(rawoutput)){dir.create(rawoutput)}

################################################################################
#read in other helper functions and DECLARE modeling options 
################################################################################
source(paste(codepath,"helper-functions.R",sep=""))
varkeep= c("nsalid1","death","tmean","dow","date","year","thisage","male","logpop")
varfun = "ns"  # using natural cubic spline
vardegree = 3  # not needed when varfun="ns" (already cubic. if we switched to "bs" then specify vardegree=2)

################################################################################
# READ THE METADATA
################################################################################
# This is generic code that will read from the data folders given above
# 
source(paste(codepath,"00.A.ReadData.R",sep=""))
print("Dimension of metadata")
print( dim(metadata))
# length(datasetlist)
 write.csv(metadata, file=paste(results.tables.path,scenario.running,"meta_data_used.csv",sep="")  )
# 
# ## Save all the table created above within an .Rdata file
save.image( file=outputfilename)
# 
# ## next time, we can start from here, without re-running everything (other than the paths)
# load( file=outputfilename)

###################################
# run analysis
##################################

print("RUNNING STEP 1")
#STEP  1: run city-specific analysis
# This runs all ages and age-stratified
##################################
t1=Sys.time()
source(paste(codepath,"00.B.RunCitySpecificModels.R",sep=""))
t2=Sys.time()
time.step1=t2-t1

#STEP  2: run meta analysis and comparative plots
################################## 
# can re-start from here: 
# load(file=outputfilename)
print("RUNNING STEP 2")
#t1=Sys.time()
source(paste(codepath,"00.C.RunMetaAnalyses.R",sep=""))
#t2=Sys.time()
#time.step2=t2-t1


####################################################################
#STEP  3: get attributable number and attributable fraction from the 
# two different meta-analyses run in the prior step
##################################
print("RUNNING STEP 3: attributable fractions")
#load( file=outputfilename)
# NUMBER OF SIMULATION RUNS FOR COMPUTING EMPIRICAL CI
#nsim = 3   
nsim = 1000   
  
THESE.KNOTS = KNOTS1; THESE.BOUNDS = BOUNDS1
extreme.temp=data.frame(cold=metadata$p5,heat=metadata$p95) #this defines that 'extreme' temperatures are at p5 and p95
#within the next file, the minimum of p5 and MMT is used for extreme cold
#and the maximum of p95 and MMT is used for extreme heat

#t1=Sys.time()
source(paste(codepath,"00.E.1.GetAttR-AttN.R",sep=""))  #attr and attn for analysis using single meta regression
#t2=Sys.time()


#STEP  4: Figures  
################################## 
print("RUNNING STEP 4: temperature-mortality plots")
source(paste(codepath,"00.F.Plot-Temperature-Mortality.R",sep=""))


