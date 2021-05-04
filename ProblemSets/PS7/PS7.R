library(tidyverse)
library(magrittr)
library(mice)
library(modelsummary)

wagedata <- read_csv('wages.csv')
wagedata %<>% mutate(college = as.factor(college),
                     married = as.factor(married))

# Summary table
datasummary_skim(wagedata,histogram=F,output="latex")

# Drop missing hgc and missing tenure
wagedata %<>% drop_na(hgc,tenure)

# Listwise deletion
parms.listwise <- lm(logwage ~ hgc + college + poly(tenure,2,raw=T) + age + married, data=wagedata, na.action=na.omit)
modelsummary(parms.listwise,output="latex")


# Mean imputation
wagedata %<>% mutate(logwage_mean_imp = case_when(
    !is.na(logwage) ~ logwage,
     is.na(logwage) ~ mean(wagedata$logwage,na.rm=T)
    )
)

parms.mean.imp <- lm(logwage_mean_imp ~ hgc + college + poly(tenure,2,raw=T) + age + married, data=wagedata, na.action=na.omit)
modelsummary(list(parms.listwise,parms.mean.imp),output="latex")


# Regression imputation
predvals <- predict(parms.listwise, newdata = wagedata)
wagedata %<>% mutate(logwage_pred_imp = case_when(
    !is.na(logwage) ~ logwage,
     is.na(logwage) ~ predvals
)
)

parms.regression.imp <- lm(logwage_pred_imp ~ hgc + college + poly(tenure,2,raw=T) + age + married, data=wagedata, na.action=na.omit)
modelsummary(list(parms.listwise,parms.mean.imp,parms.regression.imp),output="latex")

# MICE
wagedata.imp <- mice(wagedata[,c("logwage","hgc","college","tenure","age","married")], seed = 12345, m=20)
fit <- with(wagedata.imp, lm(logwage ~ hgc + college + poly(tenure,2,raw=T) + age + married))
parms.mice.imp <- mice::pool(fit)
modelsummary(list(parms.listwise,parms.mean.imp,parms.regression.imp,parms.mice.imp),output="latex")
modelsummary(list(parms.listwise,parms.mean.imp,parms.regression.imp,parms.mice.imp))
modelsummary(parms.mice.imp, output = "markdown", stars = TRUE)
