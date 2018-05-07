#####################################################################
### LOAD ABUNDANCE AND TRAIT TABLE AND PREPARE DATA FOR ANALYSES ####
#####################################################################


##Load necessary packages
library(ade4)

###################### LOAD THE DATA #################################

## Load full data table
fullData <- read.table("./Tables/01_Abund_Trait_Data.csv",header=TRUE,sep=',')

## Extract trait data
names(fullData)
traitData <- fullData[,6:64]
rownames(traitData) <- fullData[,1]

## Extract abundance data
abundData <- fullData[,66:ncol(fullData)]
abundData <- as.data.frame(t(abundData))
colnames(abundData) <- fullData[,1]

## Checking...
if(ncol(abundData)==nrow(traitData)){
  message("!! READY !!")
}

#################### SELECT YEARS AND REMOVE TAXA ####################

## Select years for analyses
# Retrieve years from rownames
year <- substr(rownames(abundData),nchar(rownames(abundData))-3,nchar(rownames(abundData))-2)
# First year to be included in our analyses
fromYear <- 2007
# Format the limit for subsetting
yearLim <- substr(as.character(fromYear),nchar(as.character(fromYear))-1,nchar(as.character(fromYear)))
# Subsetting the abundance data
abundData <- abundData[as.numeric(year)>=as.numeric(yearLim),]

## Taxa with 0 occurence have to be removed (it may happen with the year filter)
taxToRm <- names(abundData)[apply(abundData,2,sum)==0]

## More taxa to remove ??
fullData[1:4]
## For instance the Chironomidae
taxToRm <- c(taxToRm,"X145")

## Update the abundance and trait data frame (abundance=columns, traits=row)
abundData <- abundData[,-which(names(abundData) %in% taxToRm)]
traitData <- traitData[-which(rownames(traitData) %in% taxToRm),]

####################### AGGREGATION OF ABUNDANCE ###########################

## Aggregation of abundance have to be done depending on analysis
## Parameter to create the aggregation factor:
# 1: site X year X season (e.g for Pierre indices)
# 2: site X year (e.g. for Paillex index)
agParam <- 2

# Create to factor that will be used for aggregating
agFactor <- substr(rownames(abundData),1,nchar(rownames(abundData))-agParam)

# Aggregate abundance data
aggData <- aggregate(abundData,list(agFactor),sum)

# Get the sample codes in the right order for further matching with other tables
sampleOrder <- aggData[,1]
# ...and only keep abundance data (without the first column)
ab <- aggData[,-1]


####################### PREPARE TRAIT INFORMATION TABLE ########################

## Abundance and trait data
tr <- traitData

## Blocks of trait table
blocks <- c(4,5,7,6,10,5,4,3,2,8,5)
names(blocks) <- c("curr","sapr","size","locom","feeding","resp","disp","nbcycle","cycldur","repr","resist")

## Prepare trait fuzzy table
tr <- prep.fuzzy.var(tr,blocks)
