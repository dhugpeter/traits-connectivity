## Load abundance and trait data table for analyses ##

## Set working directory
setwd("C:/Timoner/LEBA/Article/Tableaux")

## Load full data table
fullData <- read.table("01_Abund_Trait_Data.csv",header=TRUE,sep=',')

## Extract trait data
traitData <- fullData[,6:64]
rownames(traitData) <- fullData[,1]

## Extract abundance data
abundData <- fullData[,66:ncol(fullData)]
abundData <- as.data.frame(t(abundData))
colnames(abundData) <- fullData[,1]

## Checking...
if(ncol(abundData)==nrow(traitData)){
  message("Ready !")
}
