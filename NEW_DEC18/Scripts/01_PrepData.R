#### THIS SCRIPT IS USED TO PREPARE THE DATA FOR FURTHER MODELING
#### IT NEEDS THE ABUNDANCE RAW DATA FOR TRAIT ANALYSES AND ABUNDANCE RAW DATA FOR TAXONOMIC ANALYSES.
#### IT NEEDS THE RAW HYDROLOGICAL DATA.
#### IT NEEDS THE RAW ENVIRONMENTAL DATA FOR CALCULTING THE CONNECTIVITY INDEX.

######################################################
## Load libraries
library(ade4)
library(vegan)
library(rstudioapi)

## Set working directory
current <- dirname(getSourceEditorContext()$path)
## Set working directory
setwd(paste0(current,"/../Tables"))

######################################################
## Create empty lists where we are going to store our prepared data for analyses and define parameters

# This list will contain 3 objects: a data frame with abundance used for trait analyses, a data frame with trait information and a data frame
# with info of considered taxa.
data.traits <- list()

# This list will contain 2 objects: a data frame with abundance used for taxonomical analyses, and a data frame
# with info of considered taxa (different from traits).
data.taxo <- list()

# This list will contain 2 objects: a data frame with hydrological data and a data frame of environmental variables (for Paillex index).
data.env <- list()

# This list will contain 3 objects: vectors of diversity responses
data.div <- list()

## Taxa to be removed (can be a vector of multiple taxa; can be genus, family or species)
rmTaxName <- "Chironomidae"

######################################################
## Check for matching samples between hydro, environmental and fauna data

# Load samplings for which we have hydro information
hydro <- read.csv("hydro_data.csv",header = TRUE, stringsAsFactors = FALSE)
samp.hydro <- hydro$ID

# Load samplings for which we have fauna information
fauna <- read.csv("fauna_tax.csv",header=TRUE,stringsAsFactors = FALSE)
samp.fauna <- colnames(fauna)[-c(1:5)]

# Load samplings for which we have environmental information
connect <- read.csv("connect_data.csv",header = TRUE, stringsAsFactors = FALSE)
samp.connect <- connect$ID

# The follwing samplings were in the hydro table and not in the fauna table; we remove them.
samp.hydro[!samp.hydro %in% samp.fauna]
# Update samplings
samp <- samp.hydro[samp.hydro %in% samp.fauna]

# The follwing samplings were in the fauna table and not in the new hydro table; we remove them.
samp.fauna[!samp.fauna %in% samp]
# Samplings after 2006 that we are going to remove:
samp.fauna[!samp.fauna %in% samp][!grepl("03|04|05|06",samp.fauna[!samp.fauna %in% samp])]
# Update samplings
samp <- samp.fauna[samp.fauna %in% samp]

# The follwing samplings were in the environemntal table and not in the previously selected samplings (normally, none).
samp[!samp.fauna %in% samp.connect]
# Update samplings
samp <- samp.connect[samp.connect %in% samp]

# Reorder and keep sampling info
samp <- samp[order(samp)]
samp.info <- hydro[match(samp,hydro$ID),1:6]
rownames(samp.info) <- 1:nrow(samp.info)

######################################################
## Loop over trait and taxonomic categories (both have different taxa and thus abundances) and keep outputs in the lists
for(type in c("trait","tax")){
  ## Read the fauna table for trait and taxon analyses
  fauna <- read.csv(paste0("fauna_",type,".csv"),header=TRUE,stringsAsFactors = FALSE)
  ## Keep taxon information with code
  taxon <- fauna[,1:5]
  ## If trait data
  if(type=="trait"){
    ## Column number for separation between traits info and sampling
    col.sep <- grep("SEPARATION",colnames(fauna))
    ## Abundance
    ab <- fauna[,(col.sep+1):ncol(fauna)]
    # data.traits[["values"]] <- fauna[,6:(col.sep-1)]
  }else{
    ab <- fauna[,6:ncol(fauna)]
  }
  ab <- t(ab)
  ## Samplings + ordering
  ab <- ab[match(samp,rownames(ab)),]
  ## Retrieve taxon codes for column names
  colnames(ab) <- taxon$Code
  rownames(ab) <- NULL # remet les num??ros de ligne
  ## Taxa with 0 occurence have to be removed (it may happen with the year filter)
  taxToRm <- as.character(taxon$Code[apply(ab,2,sum)==0])
  ## Other taxa to be removed like the Chironomidae ?
  for(lev in c("Family","Genus","Species")){
    for(sp in rmTaxName){
      if(length(length(taxon$Code[which(taxon[,lev]==sp)])>0)){
        taxToRm <- c(taxToRm,as.character(taxon$Code[which(taxon[,lev]==sp)]))
      }
    }
  }
  ## In case we have the same information for multiple levels (eg. Chironomidae)
  taxToRm <- unique(taxToRm)
  ab <- ab[,!(colnames(ab) %in% taxToRm)]
  ## Save data
  if(type=="trait"){
    data.traits[["values"]] <- fauna[!(fauna$Code %in% taxToRm),6:(col.sep-1)]
    data.traits[["taxa"]] <- taxon[taxon$Code %in% colnames(ab),]
    data.traits[["abundance"]] <- as.data.frame(ab)
  }else{
    data.taxo[["taxa"]] <- taxon[taxon$Code %in% colnames(ab),]
    data.taxo[["abundance"]] <- as.data.frame(ab)
  }
}

## Prepare environmental/hydro tables
for(type in c("hydro","connect")){
  ## Read the fauna table for trait and taxon analyses
  env <- read.csv(paste0(type,"_data.csv"),header=TRUE,stringsAsFactors = FALSE)
  env <- env[match(samp,env[,"ID"]),]
  data.env[[type]] <- env
}

## Remove intermediate objects
rm(list=c("ab","type","col.sep","fauna","taxon","taxToRm","lev","sp","env","connect","hydro","samp.fauna","samp.hydro","samp.connect","samp"))

## Some tests
nrow(data.taxo[["taxa"]])==ncol(data.taxo[["abundance"]])
nrow(data.traits[["taxa"]])==nrow(data.traits[["values"]])
nrow(data.traits[["taxa"]])==ncol(data.traits[["abundance"]])
nrow(data.taxo[["abundance"]])==nrow(data.env[["hydro"]])

######################################################
## Prepare trait matrix
# Take out some trait categories (optional); if so, be careful with the blocks of trait table
trToRm <- c("ves")
trToRm <- NULL
tr <- data.traits$values[,!(colnames(data.traits$values) %in% trToRm)]

## Blocks of trait table
colnames(data.traits$values)
blocks <- c(4,5,7,6,10,5,4,3,2,8,5)
names(blocks) <- c("curr","sapr","size","locom","feeding","resp","disp","nbcycle","cycldur","repr","resist")

## Prepare trait fuzzy table
library(ade4)
tr <- prep.fuzzy.var(tr,blocks)

######################################################
### Functional diversity
## Functional distances between taxa
fca.traits <- dudi.fca(tr,scannf=FALSE,nf=5)
fca.dist <- dist.dudi(fca.traits)

## Rao diversity
rao <- dpcoa(data.traits$abundance,fca.dist,scannf = FALSE,nf=2)
data.div[["rao"]] <- rao$RaoDiv

######################################################
### Rarefied taxonomic richness
sampleSize <- min(apply(data.taxo$abundance,1,sum))
# sampleSize <- 50
data.div[["rich"]] <- rarefy(data.taxo$abundance,sample=sampleSize,MARGIN=1)

######################################################

data.div$rich
data.div$rao
plot(data.div$rich,data.div$rao)
