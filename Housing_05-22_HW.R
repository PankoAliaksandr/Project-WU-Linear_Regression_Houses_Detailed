train <- read.csv2(file.choose(), header=TRUE, sep=",", na.strings="")

train["NbrBrkSide"] <- train$Neighborhood == "BrkSide"
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
                             NbrBrkSide +
                             Fireplaces +
                             as.factor(Zone) +
                             log(X1stFlrSF),train)

summary(regr)

# Now let's add factor GarageType
regr <- lm(log(SalePrice) ~  as.factor(GarageType) +
                             log(YearBuilt) +
                             log(LotArea) +
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrBrkSide +
                             Fireplaces +
                             as.factor(Zone) +
                             log(X1stFlrSF),train)

summary(regr)

# We clearly see that only the category GarageTypeNA (No Garage) is statistically
# significant. Keeping in mind that the reference category here is Attchd
# (Attached to home) we conclude that the garage type itself is not statistically
# significant, it matters only whether there is a garage or not. 
# Anyway, let's try to add GarageCars:

regr <- lm(log(SalePrice) ~  GarageCars +
                             as.factor(GarageType) +
                             log(YearBuilt) +
                             log(LotArea) +
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrBrkSide +
                             Fireplaces + 
                             as.factor(Zone) + 
                             log(X1stFlrSF),train)

summary(regr)

# We see that all levels of GarageType become statistically insignificant,
# while GarageCars is significant. So it is impossible to distinguish among different
# types (attached, detached, etc.), and the capacity of a garage (number of cars).
# Let's try to create a dummy variable GarageTypeYN which is assigned 1 if there is 
# a garage and 0 if there is no garage.

train["GarageTypeYN"] <- train$GarageType != "NA"

regr <- lm(log(SalePrice) ~  GarageCars +
                             as.factor(GarageTypeYN) +
                             log(YearBuilt) +
                             log(LotArea) +
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrBrkSide +
                             Fireplaces +
                             as.factor(Zone) + 
                             log(X1stFlrSF),train)

summary(regr)

# We see that if we include both factors, GarageTypeYN is insignificant.
# Let's evaluate whether GarageTypeYN can be statistically significant if we use only
# this factor, and if so, evaluate which factor - GarageType or GarageCars - provides
# better model. So let's analyize regressions where only one factor is included:

regr <- lm(log(SalePrice) ~  as.factor(GarageTypeYN) +
                             log(YearBuilt) +
                             log(LotArea) +
                             log(LivAreaSF) +
                             OverallQual +
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrBrkSide + 
                             Fireplaces + 
                             as.factor(Zone) + 
                             log(X1stFlrSF),train)

summary(regr)

regr <- lm(log(SalePrice) ~  GarageCars +
                             log(YearBuilt) +
                             log(LotArea) +
                             log(LivAreaSF) +
                             OverallQual + 
                             OverallCond +
                             NbrCrawfor +
                             NbrStoneBr +
                             NbrBrkSide + 
                             Fireplaces +
                             as.factor(Zone) +
                             log(X1stFlrSF),train)

summary(regr)

# It is clear that in both cases a garage factor becomes statistically significant.
# However, model with GarageCars has larger R^2.
# Therefore, we can conclude, that if we know the size of garage in car capacity,
# it makes more sense to use it in the model rather than just the information whether
# there is a garage or not. Or, simply speaking, the car capacity really matters.
# In a nutshell, our results also fit some common sense that the utility of a garage
# is really determined by how many cars can be stored there, and it is way more
# important than whether the garage is attached or detached to home, etc.

# Taking into account our model specification, the answer to the question,
# how much we would spend on building a garage before selling a house,
# at first sight would be: actual_price * (e^(0.067039*planned_car_capacity) - 1)
# It is clear that it depends on which car capacity we would like to make.

# However, in order to make our investments safer, we have to take into account that
# GarageCars coefficient is itself a random variable, so we need to know
# its 95\% confidence interval:

confint(regr, 'GarageCars', level=0.95)

# Note that the coefficient can fluctuate considerably - by approximately +-29\% 
# from its estimated value:

qt(0.975, 714)*summary(regr)$coefficients[2,2]/summary(regr)$coefficients[2,1]*100

# So, it would be more reasonable to calculate maximum spending using the lower bound
# of the 95\% confidence level of the coefficient:
# actual_price * (e^(0.04736421*planned_car_capacity) - 1)
# Interpretation: taking 95\% confidence interval, if garage capacity increases by 1 car,
# the expected increase of the house price is at least approximately 4.736421\%.