# Set Seet
set.seed(77)

library(tidyverse)

df <- vroom::vroom('./536/Hw2/STAT536HW2/Rivers.csv')
meta <- vroom::vroom('./536/Hw2/STAT536HW2/Metadata.csv')

# Some EDA

sd_values <- sapply(df, sd, na.rm = TRUE)
print(sort(sd_values))
df <- df %>% select(-meanPercentDC_VeryPoor, -meanPercentDC_Well) # get rid of zero variance variables

# Lasso Selection & Fitting
y <- df$Metric
X <- df %>% select(-Metric)

library(glmnet)
lasso.model = glmnet(x=X,y=y, alpha = 1)

plot(lasso.model)

lasso.cv = cv.glmnet(x=as.matrix(X),y=y, alpha = 1)

plot(lasso.cv)

lambda_1se <- lasso.cv$lambda.1se

betahats_1se <- coef(lasso.cv, s=lambda_1se)

betahats_1se_df <- as.data.frame(as.matrix(betahats_1se))
colnames(betahats_1se_df) <- c("Coefficients")
non_zero_betahats_1se_df <- betahats_1se_df[betahats_1se_df$Coefficients != 0, , drop = FALSE]
print("Non-zero coefficients for lambda.1se:")
print(non_zero_betahats_1se_df)


river_lm_lasso_selection <- lm(Metric ~ gord + MeanPrec07 + CumPrec07 + CumPrec04 + bio10 + bio15 + 
                                 bio18 + cls1 + cls2 + cls5 + cls8 + meanPercentDC_ModeratelyWell + 
                                 meanPercentDC_Poor + meanPercentDC_SomewhatExcessive + Lon, data = df)

summary(river_lm_lasso_selection)

# PCA Fitting
library(pls)

model.cv = pcr(y ~ as.matrix(X), data=df, validation="CV", scale=T)

summary(model.cv)

validationplot(model.cv)

plot(model.cv, "validation", val.type = "R2", legendpos = "bottomright")

model.2 = pcr(y ~ as.matrix(X), data=df, ncomp=2, scale=T)
summary(model.2)

# Forward Selection Model
# Fit regression with all variables, which is represented by the "."
fullmodel = lm(y ~ .,data=X)
summary(fullmodel) # there's summary again!!

# Stepwise selection
library(MASS)
model2 = stepAIC(fullmodel,direction = "forward")
summary(model2)

# PROBLEM: there is multicollinearity (4 coefficients 
# are not defined due to perfect multicollinearity or 
# lack of information to estimate them)

# Check for perfectly colinear variables
alias(model2)

# It looks like there is a lot of colinearity with the monthly and annual average
# temp, prec, and cumulative prec.

# I drop the monthly variables, in hopes that the annual variables capture
# the most of the variation and to simplify the model.

# bio3 and bio7 seem to be comprised of other bio variables, so drop those

# Install and load 'car' package for VIF
install.packages("car")
library(car)

# Model with annual data
model_revised <- lm(y ~ strmOrder + Magnitude + strmDrop + length_km + area_sqkm + 
                      drain_den + gelev_m + garea_sqkm + gord + PathLength + TotalLength + 
                      MeanTempAnn + MeanPrecAnn + CumPrecTotal + 
                      bio1 + bio2 + bio4 + bio5 + bio6 + bio8 + bio9 + bio10 + 
                      bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19 + 
                      cls1 + cls10 + cls11 + cls12 + cls2 + cls3 + cls4 + cls5 + cls6 + 
                      cls7 + cls8 + cls9 + Dam_SurfaceArea + Dam_Count + 
                      HydroLakes_Area_sqkm + MeanPopden_2000 + MeanPopden_2005 + 
                      MeanPopden_2010 + MeanPopden_2015 + MeanHumanFootprint + 
                      meanPercentDC_Imperfectly + meanPercentDC_ModeratelyWell + 
                      meanPercentDC_Poor + meanPercentDC_SomewhatExcessive + Lon + Lat,
                    data = X)


summary(model_revised)

# Check VIF for multicollinearity
vif_values <- sort(vif(model_revised), decreasing = TRUE)

# Print VIF values
print(vif_values)

# You might want to remove predictors with a VIF > 10

# Model with monthly data
model_revised <- lm(y ~ strmOrder + Magnitude + strmDrop + length_km + area_sqkm + 
                      drain_den + gelev_m + garea_sqkm + gord + PathLength + TotalLength + 
                      MeanTemp01 + MeanTemp02 + MeanTemp03 + MeanTemp04 + MeanTemp05 + 
                      MeanTemp06 + MeanTemp07 + MeanTemp08 + MeanTemp09 + MeanTemp10 + 
                      MeanTemp11 + MeanTemp12 + MeanPrec01 + MeanPrec02 + MeanPrec03 + 
                      MeanPrec04 + MeanPrec05 + MeanPrec06 + MeanPrec07 + MeanPrec08 + 
                      MeanPrec09 + MeanPrec10 + MeanPrec11 + MeanPrec12 + 
                      CumPrec01 + CumPrec02 + CumPrec03 + CumPrec04 + CumPrec05 + CumPrec06 + 
                      CumPrec07 + CumPrec08 + CumPrec09 + CumPrec10 + CumPrec11 + CumPrec12 + 
                      bio1 + bio2 + bio4 + bio5 + bio6 + bio8 + bio9 + bio10 + 
                      bio11 + bio12 + bio13 + bio14 + bio15 + bio16 + bio17 + bio18 + bio19 + 
                      cls1 + cls10 + cls11 + cls12 + cls2 + cls3 + cls4 + cls5 + cls6 + 
                      cls7 + cls8 + cls9 + Dam_SurfaceArea + Dam_Count + 
                      HydroLakes_Area_sqkm + MeanPopden_2000 + MeanPopden_2005 + 
                      MeanPopden_2010 + MeanPopden_2015 + MeanHumanFootprint + 
                      meanPercentDC_Imperfectly + meanPercentDC_ModeratelyWell + 
                      meanPercentDC_Poor + meanPercentDC_SomewhatExcessive + Lon + Lat,
                    data = X)


summary(model_revised)

# Check VIF for multicollinearity
vif_values <- sort(vif(model_revised), decreasing = TRUE)

# Print VIF values
print(vif_values)

# Stepwise selection pt 2
empty_model = lm(y ~ 1, data = X)
step_model = stepAIC(empty_model, scope = ~ strmOrder + Magnitude + strmDrop + length_km + area_sqkm + 
                       drain_den + gelev_m + garea_sqkm + gord + PathLength + 
                       TotalLength + MeanTemp01 + MeanTemp02 + MeanTemp03 + 
                       MeanTemp04 + MeanTemp05 + MeanTemp06 + MeanTemp07 + 
                       MeanTemp08 + MeanTemp09 + MeanTemp10 + MeanTemp11 + 
                       MeanTemp12 + MeanTempAnn + MeanPrec01 + MeanPrec02 + 
                       MeanPrec03 + MeanPrec04 + MeanPrec05 + MeanPrec06 + 
                       MeanPrec07 + MeanPrec08 + MeanPrec09 + MeanPrec10 + 
                       MeanPrec11 + MeanPrec12 + MeanPrecAnn + CumPrec01 + 
                       CumPrec02 + CumPrec03 + CumPrec04 + CumPrec05 + 
                       CumPrec06 + CumPrec07 + CumPrec08 + CumPrec09 + 
                       CumPrec10 + CumPrec11 + CumPrec12 + CumPrecTotal + 
                       bio1 + bio2 + bio3 + bio4 + bio5 + bio6 + bio7 + 
                       bio8 + bio9 + bio10 + bio11 + bio12 + bio13 + bio14 + 
                       bio15 + bio16 + bio17 + bio18 + bio19 + cls1 + cls10 + 
                       cls11 + cls12 + cls2 + cls3 + cls4 + cls5 + cls6 + 
                       cls7 + cls8 + cls9 + Dam_SurfaceArea + Dam_Count + 
                       HydroLakes_Area_sqkm + MeanPopden_2000 + MeanPopden_2005 + 
                       MeanPopden_2010 + MeanPopden_2015 + MeanHumanFootprint + 
                       meanPercentDC_Imperfectly + meanPercentDC_ModeratelyWell + 
                       meanPercentDC_Poor + meanPercentDC_SomewhatExcessive + 
                       Lon + Lat,
                     direction = "forward")

summary(step_model)
