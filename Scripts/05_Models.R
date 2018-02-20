#####################################################################
############## PLOT AND PERFORM SIMPLE REGRESSIONS ##################
#####################################################################

## Set the predictor variable
pred <- paillex

## Set the explained variable
expl <- raoDiv
# expl <- Merritt[[1]]

## Plot and simple linear regression without removing outlier
plot(expl~pred,pch=16)
abline(lm(expl~pred),col="red")
summary(lm(expl~pred))

## Remove outliers from explained variable (lines from predictor variable have to be removed as well)
pred_b <- pred[!expl>quantile(expl,probs=0.75)+1.5*IQR(expl) & !expl<quantile(expl,probs=0.25)-1.5*IQR(expl)]
expl_b <- expl[!expl>quantile(expl,probs=0.75)+1.5*IQR(expl) & !expl<quantile(expl,probs=0.25)-1.5*IQR(expl)]

## Plot and simple linear regression after removing outliers
plot(expl_b~pred_b,pch=16)
abline(lm(expl_b~pred_b),col="red")
summary(lm(expl_b~pred_b))

