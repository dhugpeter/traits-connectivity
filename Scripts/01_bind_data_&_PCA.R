setwd("C:/Users/marle/Dropbox/Traits_PT_DHP_PM")

# The two data table to bind
paillex <- read.csv("02_connec_paillex.csv", sep=";")
riquiermarle <- read.csv("03_hydro_data_clean.csv", sep=";")

# All variables
connectivity_all <- merge(paillex, riquiermarle, by.x=c("Station","Site","Year"), 
                          by.y=c("Station","Site","Year"))

# Delete NA (gap in discharge time series)
delete.na <- function(DF, n=0) {DF[rowSums(is.na(DF)) <= n,]} #fonction pour supprimer lignes avec des na
connectivity_all <- delete.na(connectivity_all)

##########################################################################################
#Selon dernier article en cours de rédaction, les 3 variables hydro qui expliquent
#le mieux les assemblages d'invertébrés sont Of4m (Overflows), Bny (Backflows) et 
#Le2m (Undisturbed state, low flow ; Marle, unpublish.)
#Le tableau suivant, se resteindra à ces 3 variables hydro

connectivity_sel <- connectivity_all[,c("Sector.x","Station","Site","Year","Season","ID","Date_sample", #sample info
                                        "cond","granu","veg","mo","connect", #Paillex data
                                        "Of4m","Bny","Le2m")] #Riquier/Marle data


summary(connectivity_sel)

#Changer rownames
rownames(connectivity_sel) <- connectivity_sel[,"ID"]

#Standardisation des variables hydro
connectivity_sel[,13:15] <- scale(connectivity_sel[,13:15])


# Visualisation et correlations

pairs(connectivity_sel[,c(8:15)],
      lower.panel = NULL, 
      col = as.numeric(connectivity_sel$Season))



# PCA Paillex

Paillex.pca <- princomp(connectivity_sel[,8:11]) #sans indice de connectivité
biplot(Paillex.pca, cex=0.5)

Paillex.pca <- princomp(connectivity_sel[,8:12]) #avec indice de connectivité
biplot(Paillex.pca, cex=0.5)


# PCA Riquier & Marle

Marle.pca <- princomp(connectivity_sel[,13:15])
biplot(Marle.pca, cex=0.5)


