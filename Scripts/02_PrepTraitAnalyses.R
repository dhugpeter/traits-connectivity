## 01_LoadData.R has to be run to load the abundance and trait tables ##
## Prepare abundance and trait data for further analyses

## Abundance and trait data
Tr <- traitData

## Prepare the trait data
# Blocks of trait table
blocks <- c(4,5,7,6,10,5,4,3,2,8,5)
names(blocks) <- c("curr","sapr","size","locom","feeding","resp","disp","nbcycle","cycldur","repr","resist")

## Prepare trait fuzzy table
Tr <- prep.fuzzy.var(Tr,blocks)