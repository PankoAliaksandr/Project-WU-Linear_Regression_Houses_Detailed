train <- read.csv2(file.choose(), header=TRUE, sep=",", na.strings="")

train["NbrCrawfor"] <- train$Neighborhood == "Crawfor"
train["NbrStoneBr"] <- train$Neighborhood == "StoneBr"
train["NbrEdwards"] <- train$Neighborhood == "Edwards"
train["BldgTypeDuplex"] <- train$BldgType == "Duplex"
train["BsmtExposureGd"] <- train$BsmtExposure == "Gd"
train["FoundationPConc"] <- train$Foundation == "PConc"

# Let's start with the regression where all factors in the start of analysis seem to be statistically significant:
regr <- lm(log(SalePrice) ~  log(YearBuilt) +
                             log(LotArea) + 
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrEdwards +
                             Fireplaces +
                             log(X1stFlrSF) +
                             GarageCars +
                             BldgTypeDuplex +
                             BsmtExposureGd +
                             FoundationPConc
           ,train)
summary(regr)

# Now let's perform the standard and the studentized Breusch-Pagan test to check
# the homoscedasticity:

car::ncvTest(regr)
lmtest::bptest(regr)

# The tests clearly show that homoscedasticity null hypothesis should be rejected,
# and we must accept alternative hypothesis of heteroscedasticity.
# Therefore the estimated standard errors of coefficients are biased and
# the associated t- and F-statistics are incorrect.
# So let's compute heteroscedasticity-consistent standard errors using the R-package 'AER'

library(AER)

coefftest <- coeftest(regr, vcov = vcovHC(regr, type = "HC0"))
print(coefftest)
summary(regr)$coefficients[,2]/coefftest[,2]

# It can be seen that standard errors of the intercept and variables:
# log(YearBuilt), log(LotArea), OverallQual, OverallCond, NbrCrawfor, NbrStoneBr,
# BldgTypeDuplex and FoundationPConc were overestimated.

# At the same time standard errors of variables:
# log(LivAreaSF), NbrEdwards, Fireplaces, log(X1stFlrSF), GarageCars and BsmtExposureGd
# were underestimated.

# Especially heavy underestimation takes place for:
# Fireplaces, log(LivAreaSF), BsmtExposureGd, GarageCars (by almost 30%),
# and NbrEdwards (by almost 2 times).

# But overestimation is not drastic, in most cases by no more than few percent
# and in any single case by no more than 14% (in case of FoundationPConc).

# According to recalculation of p-values, variables Fireplaces and NbrEdwards become statistically insignificant.
# However, p-value for Fireplaces becomes just slightly more than 0.05, and 0.07 for NbrEdwards is also not that drastic.
