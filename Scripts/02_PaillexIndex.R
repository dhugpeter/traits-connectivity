## Calculate the connectiviy index (according to Paillex)

## Set working directory
setwd("C:/Timoner/LEBA/Article/Tableaux")

## Load required library
library(ade4)

## Load environmental table
env <- read.table("../Tables/env_variables.txt",header=TRUE)

## PCA on substrate diversity, submerged vegetation, conductance and organic matter
envPCA <- dudi.pca(env[,3:6],scannf=FALSE,nf = 2)

## Get the axis 1 coordinates (the less negative the more connected) as connectivity index
paillex <- -envPCA$li[,1]

## Rescale the connectivity index between 0 and 1
paillex <- (paillex-min(paillex))/(max(paillex)-min(paillex))

