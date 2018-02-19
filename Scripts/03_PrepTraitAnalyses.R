## 01_LoadData.R has to be run to load the abundance and trait tables ##
## Prepare abundance and trait data for further analyses

## Prepare the abundance data
# Create a factor to aggregate the data (site x year)
siteYear <- substr(rownames(abundData),1,nchar(rownames(abundData))-2)
# Aggregate abundance data
aggData <- aggregate(abundData,list(siteYear),sum)
names(aggData)[1] <- "SiteYear"
Ab <- aggData

## Prepare the trait data
# Blocks of trait table
blocks <- c(4,5,7,6,10,5,4,3,2,8,5)
names(blocks) <- c("curr","sapr","size","locom","feeding","resp","disp","nbcycle","cycldur","repr","resist")
Tr <- traitData

## Take out taxa
# Check taxon indices
fullData[,1:4]
# For instance, Chironomidae
Ab <- Ab[,-145]
Tr <- Tr[-145,]

## Prepare trait fuzzy table
Tr <- prep.fuzzy.var(Tr,blocks)
