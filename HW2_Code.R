# Set Seet
set.seed(77)

library(tidyverse)

df <- vroom::vroom('Rivers.csv')
meta <- vroom::vroom('Metadata.csv')

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



