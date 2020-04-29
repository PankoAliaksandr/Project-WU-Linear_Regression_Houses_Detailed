train <- read.csv2(file.choose(), header=TRUE, sep=",", na.strings="")

#a)
train["NbrCrawfor"] <- train$Neighborhood == "Crawfor"
train["NbrStoneBr"] <- train$Neighborhood == "StoneBr"

# Let's start with the regression where all factors are statistically significant
regr <- lm(log(SalePrice) ~  log(YearBuilt) +
                             log(LotArea) + 
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             Fireplaces +
                             log(X1stFlrSF),
           train)

summary(regr)
# Let's take as omitted variables log(LivAreaSF), OverallQual and Fireplaces
# So the reduced model is:
regr_r <- lm(log(SalePrice) ~  log(YearBuilt) +
             log(LotArea) + 
             #log(LivAreaSF) +
             #OverallQual +
             OverallCond +
             NbrCrawfor +
             NbrStoneBr +
             #Fireplaces +
             log(X1stFlrSF),
             train)

summary(regr_r)
AIC(regr)
AIC(regr_r)
BIC(regr)
BIC(regr_r)
# Both AIC and BIC drastically increase when we omit these variables, and $R^2$ with adjusted $R^2$ heavily drops.
# We clearly lose information by omitting these variables.
# The bias would be
X1 <- cbind(log(train$YearBuilt),log(train$LotArea),train$OverallCond,train$NbrCrawfor,train$NbrStoneBr,log(train$X1stFlrSF))
X2 <- cbind(log(train$LivAreaSF),train$OverallQual,train$Fireplaces)
bias1 <- qr.solve(t(X1) %*% X1) %*% t(X1) %*% X2 %*% regr$coefficients[c(4,5,9)]
print(bias1)

# b) Let's add to our basic regression GarageArea, GarageCars and both of them
regrGA <- lm(log(SalePrice) ~  log(YearBuilt) +
               log(LotArea) + 
               log(LivAreaSF) +
               OverallQual +
               OverallCond +
               NbrCrawfor +
               NbrStoneBr +
               Fireplaces +
               log(X1stFlrSF) +
               GarageArea,
             train)

summary(regrGA)

regrGC <- lm(log(SalePrice) ~  log(YearBuilt) +
                               log(LotArea) + 
                               log(LivAreaSF) +
                               OverallQual +
                               OverallCond +
                               NbrCrawfor +
                               NbrStoneBr +
                               Fireplaces +
                               log(X1stFlrSF) +
                               GarageCars,
             train)

summary(regrGC)

regrGBoth <- lm(log(SalePrice) ~  log(YearBuilt) +
               log(LotArea) + 
               log(LivAreaSF) +
               OverallQual +
               OverallCond +
               NbrCrawfor +
               NbrStoneBr +
               Fireplaces +
               log(X1stFlrSF) +
               GarageArea +
               GarageCars,
             train)

summary(regrGBoth)

# It can be seen that if either GarageArea or GarageCars is used, it is statistically significant.
# It fits common sense that some factor reflecting the presense of a garage should influence on the price of a house,
# especially in Ames, Iowa, since small American towns are known for not having extensive
# public transport system (if any).
# However, if we use both factors, GarageArea becomes statistically insignificant.
# Therefore, we should use only one of them.
# Both AIC and BIC criteria clearly demonstrate that we should use the model with GarageCars
AIC(regrGA)
AIC(regrGC)
AIC(regrGBoth)
BIC(regrGA)
BIC(regrGC)
BIC(regrGBoth)
# It corresponds to some common sense that the capacity of garage in the terms of the number of cars
# which could be parked there better reflects the utility of the garage than its area.
# In principle, if the garage is badly planned, larger area may not increase its utility.
# On a relatively small data set it is hardly possible to distinguish the effects of car capacity and area.
# Anyway, let's compare the biases which arise if we remove from the full model either GarageCars or GarageArea:
X1GC <- cbind(log(train$YearBuilt),log(train$LotArea),log(train$LivAreaSF),train$OverallQual,train$OverallCond,train$NbrCrawfor,train$NbrStoneBr,log(train$X1stFlrSF),train$Fireplaces,train$GarageArea)
X2GC <- cbind(train$GarageCars)
biasGC <- qr.solve(t(X1GC) %*% X1GC) %*% t(X1GC) %*% X2GC %*% regrGBoth$coefficients[12]
print(biasGC)

X1GA <- cbind(log(train$YearBuilt),log(train$LotArea),log(train$LivAreaSF),train$OverallQual,train$OverallCond,train$NbrCrawfor,train$NbrStoneBr,log(train$X1stFlrSF),train$Fireplaces,train$GarageCars)
X2GA <- cbind(train$GarageArea)
biasGA <- qr.solve(t(X1GC) %*% X1GC) %*% t(X1GC) %*% X2GC %*% regrGBoth$coefficients[11]
print(biasGA)

# It is clear that bias for all coefficients is considerably larger if we omit GarageCars than if we omit GarageArea.
# It also confirms our conclusions that we should use the model with GarageCars.