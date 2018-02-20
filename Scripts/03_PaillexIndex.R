#####################################################################
############ CALCULATE PAILLEX CONNECTIVITY INDEX ##################
#####################################################################

## 01_LoadData.R has to be run to prepare abundance and trait data
## yearLim and sampleOrder objects are needed

## Load required library
library(ade4)

## Load environmental table
env <- read.table("../Tables/02_env_variables.txt",header=TRUE)

## Select years
env <- subset(env,env$an>=as.numeric(yearLim))

## Reorder with match function
env <- env[match(sampleOrder,rownames(env)),]

## Now we have the right order, we calculate the index of connectivity from Paillex
## PCA on substrate diversity, submerged vegetation, conductance and organic matter
envPCA <- dudi.pca(env[,3:6],scannf=FALSE,nf = 2)

## Plot of factorial maps with representation of point classes (site)
s.class(envPCA$li,env$site)

## Get the axis 1 coordinates (the less negative the more connected) as connectivity index
paillex <- -envPCA$li[,1]

## Rescale the connectivity index between 0 and 1 (optional. Emmanuel calculated it this way.)
paillex <- (paillex-min(paillex))/(max(paillex)-min(paillex))

