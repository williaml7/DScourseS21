# ECON 5253: PS7 R script
# William Lorton

library(tidyverse)

# Packing for imputing missing data.
library(mice)
# Package for converting summary stat tables and output from stat models into Latex tables.
library(modelsummary)

# load wage data
# data on 2246 women working in the u.s. in 1988
# tenure --> how long in years each person had been working for their employer
# hgc --> how many years of schooling each woman had completed
df.raw <- read.csv("wages.csv")

# drop observations where hgc or tenure are missing
df.na <- df.raw %>% drop_na(hgc, tenure)

# use modelsummary to produce a summary table of this data frame
# Putting latex output in my overleaf document directly
datasummary_skim(df.na, out = "latex")
# see it in Rstudio
datasummary_skim(df.na, out = "markdown")
# 25% of the logwage observations are missing.
# The logwage variable is most likely to be Missing Not At Random (MNAR)
# Whether someone is working or not (earning a wage) is an endogenous choice; 
# it's made for a variety of reasons that we don't have any real insight into. 
# Also, there's no indication that the values are missing because the person who compiled 
# this data set, say, randomly forgot to input values for these particular observations.

# Performing various imputation methods for missing logwage observations.

# For each imputation method, we estimate the following linear regression model:

# logwage_i = β0 + β1hgc_i + β2college_i + β3tenure_i + β4tenure^2_i + β5age_i + β6married_i + ε_i

# The coefficient of interest is β1 (returns to schooling in terms of earnings from work).

# Make sure that college and married are interpreted as factor variables.
df.na$college <- as.factor(df.na$college)
df.na$married <- as.factor(df.na$married)

# Imputation via listwise deletion on the logwage variable (assumes MCAR):
df.impute1 <- df.na %>% drop_na(logwage)
datasummary_skim(df.impute1, out = "markdown")

est.impute1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = df.impute1)

# Since we are only looking at those with non-missing earnings, we are missing out on a large portion
# of the data. As stated before, earnings are not randomly assigned.

# Mean imputation (fill in NAs with mean(logwage)):
logwage.mean <- mean(df.impute1$logwage)
df.impute2 <- df.na %>% mutate(logwage2 = ifelse(is.na(logwage), logwage.mean, logwage))

est.impute2 <- lm(logwage2 ~ hgc + college + tenure + I(tenure^2) + age + married, data = df.impute2)

# notice that mean imputation results in the hgc estimate falling; we may be understating the return to
# school in the sense that many of the women with no wage data completed college and choose to be 
# stay at home mothers. By assigning to these people the mean wage by default, we are understating 
# their true earning potential with respect to their education.

# Impute missing log wages as their predicted values from the listise deletion imputed regression above
# (assumes MAR); this is the "matching" form of imputation; find a "donor" for the observation
# with missing wage data (one with "similar enough" other values).
# predict gives vector of predicted values from est.impute1 using df.na.
df.impute3 <- df.na %>% mutate(logwage2 = ifelse(is.na(logwage), predict(est.impute1, newdata = df.na), logwage))

est.impute3 <- lm(logwage2 ~ hgc + college + tenure + I(tenure^2) + age + married, data = df.impute3)

# Using mice package to perform multiple imputation.
# impute five times.
df.na_mice <- mice(data = df.na, m = 5)
# estimate models.
est.impute4 <- with(df.na_mice, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
est.impute4 <- mice::pool(est.impute4)

# Create regression table containing estimates from all four linear regression models:
modelsummary(list(est.impute1, est.impute2, est.impute3, est.impute4), output = "latex")
modelsummary(list(est.impute1, est.impute2, est.impute3, est.impute4), output = "markdown")










