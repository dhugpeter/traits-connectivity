## 02_PrepTraitAnalyses.R has to be run to prepare abundance and trait data ##
## Load required library
library(ade4)

## Prepare the abundance data
# Create a factor to aggregate the data (e.g. site x year)
siteYear <- substr(rownames(abundData),1,nchar(rownames(abundData))-2)
# Aggregate abundance data
aggData <- aggregate(abundData,list(siteYear),sum)

# Save SiteYear in the right order and in the standard format (zeros were removed for filtering years)
Site <- substr(aggData[,1],1,nchar(aggData[,1])-2)
Year <- as.numeric(substr(aggData[,1],nchar(aggData[,1])-1,nchar(aggData[,1])))
SiteYear <- paste0(Site,Year)

# Only abundance data (without the first column)
Ab <- aggData[,-1]

## Load environmental table
env <- read.table("../Tables/02_env_variables.txt",header=TRUE)
## Select years
env <- subset(env,env$an>6)
## Create a new column matching with abundance data and having the right order
env$code <- paste0(env$site,env$an)
## Reorder with match function
env <- env[match(SiteYear,env$code),]
## Remove the code column
env <- env[,-7]

## Now we have the right order, we calculate the index of connectivity from Paillex
## PCA on substrate diversity, submerged vegetation, conductance and organic matter
envPCA <- dudi.pca(env[,3:6],scannf=FALSE,nf = 2)

## Get the axis 1 coordinates (the less negative the more connected) as connectivity index
paillex <- -envPCA$li[,1]

## Rescale the connectivity index between 0 and 1 if necessary
## (To get the same results than Emmanuel, we have to rescale and to consider every year)
# paillex <- (paillex-min(paillex))/(max(paillex)-min(paillex))


## Functional distance between taxa
fcaTax <- dudi.fca(Tr,scannf=FALSE,nf=5)
distTax <- dist.dudi(fcaTax)
labels(distTax) -> names(Ab)

## Rao indices
rao <- dpcoa(Ab,distTax,scannf = FALSE,nf=2)

raoDiv <- rao$RaoDiv





dfPaillex <- data.frame(Code=paste0(env$site,env$an),Connect=paillex)
siteYear

class(env)
dim(env)
