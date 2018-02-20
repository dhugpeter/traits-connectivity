#####################################################################
### CALCULATE FUNCTIONAL DIVERSITY INDEX (RAO) AND OTHER METRICS ####
#####################################################################

## tr and ab objects are needed (01_LoadData.R)

## Required library
library(ade4)

###################### FUNCTIONAL DIVERSITY ########################

## Functional distance between taxa
fcaTax <- dudi.fca(tr,scannf=FALSE,nf=2)
distTax <- dist.dudi(fcaTax)
labels(distTax) -> names(ab)

## Double Principle Coordinate Analysis...
rao <- dpcoa(ab,distTax,scannf = FALSE,nf=2)
# ... to get the functional diversity index (Rao index)
raoDiv <- rao$RaoDiv

###################### METRICS FROM MERRITT ########################

## Matrix multiplication
abTr <- as.matrix(ab)%*%as.matrix(tr)
abTr <- as.data.frame(abTr)

## Create an empty list for the metrics
Merritt <- list()

## Calculate the metrics...
Merritt[[1]] <- as.vector(abTr[,26]/apply(abTr[,c(27:29)],1,sum))
Merritt[[2]] <- as.vector(apply(abTr[,c(28:29)],1,sum)/(abTr[,27]))
Merritt[[3]] <- as.vector(abTr[,30]/apply(abTr[,c(23:29,31:32)],1,sum))
Merritt[[4]] <- as.vector(abTr[,44]/apply(abTr[,c(42:43)],1,sum))
Merritt[[5]] <- as.vector(abTr[,20]/apply(abTr[,c(17:19,21:22)],1,sum))
Merritt[[6]] <- as.vector(apply(abTr[,c(23,28,29)],1,sum)/apply(abTr[,c(26,27)],1,sum))
Merritt[[7]] <- as.vector(abTr[,21]/apply(abTr[,c(19,20,17,18)],1,sum))
# ... and set the names
names(Merritt) <- c("Shredder_ratio","Collector_ratio","Predator_ratio","Voltinism_ratio","Benthic_ratio","FFG_ratio","FHG_ratio")
