################################################################################
# Adapted version of the code for the analysis in:
#   "Mortality risk attributable to high and low ambient temperature: a multi-country study"
#   by: Antonio Gasparrini and collaborators
#   The Lancet - 2015; from: http://www.ag-myresearch.com/2015_gasparrini_lancet.html
#
# Update: April 23 - 2021
# * an updated version of this code, to be run with SALURBAL data
#   the changes are:
#   1. Remove models for children (keeping: all ages, 65+, <65, 50-64, <50)
#   2. Separate code so that only the 00.RunMe_Generic_AllSteps.R file needs to be modified
#      when running analysis for various mortality types.
#      The file calls in all other files (those files are all exactly the same).
#      The code should/can always be read from the shared/server drive.
#   3. Extreme temperature AttR and AttN (in addition to all heat and all cold) 
#   4. Save all output to enable calculation/drawing of lag curves
#   5. Calculate and plot lags
################################################################################
################################################################################
##DECLARE FILE DIRECTORIES GENERAL - no need to change
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
#read in other helper functions and DECLARE modeling options - NO NEED TO MODIFY THIS 
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

print("RUNNING STEP 1: fit models")
#STEP  1: run city-specific analysis
# This runs all ages and age-stratified
##################################
source(paste(codepath,"00.B.RunClusterSpecificModels-CauseSpecific.R",sep=""))
save.image( file=outputfilename)


####################################################################
#STEP 2: get attributable number and attributable fraction 
##################################
print("RUNNING STEP 2: ATTR")
nsim = 1000   
 #nsim=3

extreme.temp=data.frame(cold=metadata$p5,heat=metadata$p95) #this defines that 'extreme' temperatures are at p5 and p95
#within the next file, the minimum of p5 and MMT is used for extreme cold
#and the maximum of p95 and MMT is used for extreme heat

source(paste(codepath,"00.E.1.GetAttR-AttN-CauseSpecific.R",sep=""))  #attr and attn for analysis using single meta regression
save.image( file=outputfilename)


