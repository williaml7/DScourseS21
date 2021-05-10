# ECON 5253: Project
# May 2021
# William Lorton

library(tidyverse)
library(mice)
library(modelsummary)
library(clubSandwich)

# Purpose: reproduce results (regression and summary statistic tables) displayed in Lorton_project.pdf.
# Please see Lorton_project.R to understand how each panel data set loaded in below was created.
# This script performs imputation and produces summary statistic and regression tables for the project.

# Load BCS era data set.
load("bcs_merged_noImpute.Rda")
# Load Playoff era data set.
load("playoff_merged_noImpute.Rda")

## DETERMINING WHICH VARIABLES SHOULD BE LOGGED ##################################################################################################################################################################

# We take the log of a number of our variables since they exhibit a log-normal distribution (continuous, strictly
# positive, many observations around a relatively small value, and strongly right-skewed). Taking the log
# can also be helpful for interpretation (e.g. consider a percentage increase in applications rather than
# an increase in the number of applications). Taking the log on variables like applications and enrollment
# can also help avoid overweighting the larger schools in the sample. See the histograms and comments below.

# applications are quite right-skewed and it will be helpful to log for interpretation and balancing reasons.
hist(df.playoff.merged$applications, breaks = 50, xlab = "Applications", main = "Playoff Era")
hist(df.bcs.merged$applications, breaks = 50, xlab = "Applications", main = "BCS Era")

# Enrollment not terribly right-skewed, but will be helpful to take log for interpretation and balancing reasons.
hist(df.playoff.merged$enrollments, breaks = 50, xlab = "Enrollment", main = "Playoff Era")
hist(df.bcs.merged$enrollments, breaks = 50, xlab = "Enrollment", main = "BCS Era")

# Number of HS grads by state is strongly right-skewed, so we'll take log.
hist(df.playoff.merged$numHSgrads, breaks = 50, xlab = "HS Graduates", main = "Playoff Era")
hist(df.bcs.merged$numHSgrads, breaks = 50, xlab = "HS Graduates", main = "BCS Era")

# Might as well also take the log of median HH income as it has some of the characteristics 
# of a log-normal variable and it is also common to do so in the literature.
hist(df.playoff.merged$medianHHincome, breaks = 50, xlab = "Median household income", main = "Playoff Era")
hist(df.bcs.merged$medianHHincome, breaks = 50, xlab = "Median household income", main = "BCS Era")

### IMPUTATION AND MODEL ESTIMATION ########################################################################################################################################################################################

# First, we impute for the df.playoff.merged data set which has 21 rows with missing SAT score observations.

# Impute using linear regression predictions from regression with data set containing NAs (assumes MAR).
# Can't use school as a regressor because this drops some schools from the data set (Wake Forest,
# Kansas State, and Kansas all have missing SAT data for each year).
est.impute.listwise <- lm(avgSAT75 ~ ACTcomposite75 + wontitle + wontitleLag1 + top15 + top15Lag1 + top5 + top5Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + admitRate + yield, 
                          data = df.playoff.merged)
# Also, sometimes rows are missing BOTH ACT and SAT scores; we need a different regression model
# for this that doesn't use ACTcomposite75.
est.impute.listwise2 <- lm(avgSAT75 ~ wontitle + wontitleLag1 + top15 + top15Lag1 + top5 + top5Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + admitRate + yield, 
                          data = df.playoff.merged)
# Substitute in predictions from first regression equation estimated above where NA values are.
df.playoff.merged <- df.playoff.merged %>% mutate(avgSAT75imputed = ifelse(is.na(avgSAT75), predict(est.impute.listwise, newdata = df.playoff.merged), avgSAT75))
# Now use the second regression equation for the rows that didn't have an ACT score.
df.playoff.merged <- df.playoff.merged %>% mutate(avgSAT75imputed = ifelse(is.na(avgSAT75imputed), predict(est.impute.listwise2, newdata = df.playoff.merged), avgSAT75imputed))

# Showing number of missing values for df.playoff.merged. Variables should have 325 observations.
# 21 rows with missing SAT scores. The variable avgSAT75imputed covers for all of them.
stargazer(df.playoff.merged, type = "text", median = TRUE, omit = c("winpctLag1", "admitRate", "yield", "admissions", "ACTcomposite75"),
          title = "Numerical Summary Statistics for Playoff Era")
# stargazer(df.playoff.merged, type = "latex", median = TRUE, omit = c("winpctLag1", "admitRate", "yield", "admissions", "ACTcomposite75"),
#           title = "Numerical Summary Statistics for Playoff Era")
# None of the dummy variables have missing observations.
datasummary(wontitle + top5 + top15 ~ N, data = df.playoff.merged, output = "markdown",
            title = "Categorical Summary Statistics for Playoff Era")
# datasummary(wontitle + top5 + top15 ~ N, data = df.playoff.merged, output = "latex_tabular",
#             title = "Categorical Summary Statistics for Playoff Era")

# Now estimate the fixed effects models of interest using the imputed values.

# dependent variable log(applications):
est.playoff.applications <- lm(log(applications) ~ wontitle + wontitleLag1 + top5 + top5Lag1 + top15 + top15Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + school,
                               data = df.playoff.merged)
clust.applications <- coef_test(est.playoff.applications, vcov = "CR2", cluster = df.playoff.merged$school)

# dependent variable log(enrollments):
est.playoff.enrollments <- lm(log(enrollments) ~ wontitle + wontitleLag1 + top5 + top5Lag1 + top15 + top15Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + school,
                               data = df.playoff.merged)
clust.enrollments <- coef_test(est.playoff.enrollments, vcov = "CR2", cluster = df.playoff.merged$school)

# dependent variable avgSAT75imputed:
est.playoff.SAT <- lm(avgSAT75imputed ~ wontitle + wontitleLag1 + top5 + top5Lag1 + top15 + top15Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + school,
                              data = df.playoff.merged)
clust.SAT <- coef_test(est.playoff.SAT, vcov = "CR2", cluster = df.playoff.merged$school)

# display fixed effects regression output for Playoff era:
stargazer(est.playoff.applications, est.playoff.enrollments, est.playoff.SAT, 
          se = list(clust.applications$SE, clust.enrollments$SE, clust.SAT$SE),
          p = list(clust.applications$p_Satt, clust.enrollments$p_Satt, clust.SAT$p_Satt),
          type = "text",
          report = "vc*ps",
          title = "Playoff Era",
          omit = c("school", "year"),
          notes = "Year and school dummy coefficients are not displayed. Robust standard errors are in parentheses.",
          notes.align = 'l')
# stargazer(est.playoff.applications, est.playoff.enrollments, est.playoff.SAT, 
#           se = list(clust.applications$SE, clust.enrollments$SE, clust.SAT$SE),
#           p = list(clust.applications$p_Satt, clust.enrollments$p_Satt, clust.SAT$p_Satt),
#           type = "latex",
#           report = "vc*ps",
#           title = "Playoff Era",
#           omit = c("school", "year"),
#           notes = "Year and school dummy coefficients are not displayed. Robust standard errors are in parentheses.",
#           notes.align = 'l')


# Now, we impute for the df.bcs.merged data set.

# Duke has applications, admissions, and enrollment (erroneously) set to zero in its 2001 row For some reason, 
# IPEDS doesn't have data on Duke's admissions for 2001, nor do they have any on it for 2000, or 1999. However, 
# I found an article from April 2001 that gives some information on this. It is stated that Duke received 
# 14,647 applications, admitted 3,083, and were targeting enrollment of 1,597 of these admits. I'll just use 
# this number as the freshman enrollment for fall 2001. https://today.duke.edu/2001/04/admits406.html
df.bcs.merged[39,10:14] <- c(3083/14647, 1597/3083, 14647, 3083, 1597)

# As before, we impute using linear regression predictions from regression with data set containing NAs 
# (assumes MAR).
# Again, don't use school as a regressor since some are removed from the data set during this regression's estimation.
est.impute.listwise <- lm(avgSAT75 ~ ACTcomposite75 + wontitle + wontitleLag1 + top15 + top15Lag1 + top5 + top5Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + admitRate + yield, 
                          data = df.bcs.merged)
# Second regression equation for rows missing BOTH ACT and SAT score data.
est.impute.listwise2 <- lm(avgSAT75 ~ wontitle + wontitleLag1 + top15 + top15Lag1 + top5 + top5Lag1 + winpct + winpctLag1 + log(numHSgrads) + log(medianHHincome) + year + admitRate + yield, 
                           data = df.bcs.merged)
# Substitute in predictions from first regression equation estimated above where NA values are.
df.bcs.merged <- df.bcs.merged %>% mutate(avgSAT75imputed = ifelse(is.na(avgSAT75), predict(est.impute.listwise, newdata = df.bcs.merged), avgSAT75))
# Now use the second regression equation for the rows that didn't have an ACT score.
df.bcs.merged <- df.bcs.merged %>% mutate(avgSAT75imputed = ifelse(is.na(avgSAT75imputed), predict(est.impute.listwise2, newdata = df.bcs.merged), avgSAT75imputed))

# Showing number of missing values for df.bcs.merged. Variables should have 806 observations if complete.
# There are 72 rows with missing SAT data.
stargazer(df.bcs.merged, type = "text", median = TRUE, 
          omit = c("winpctLag1", "admitRate", "yield", "admissions", "ACTcomposite75", "winpctLag2", "winpctLag3"),
          title = "Numerical Summary Statistics for BCS Era")
# stargazer(df.bcs.merged, type = "latex", median = TRUE, 
#           omit = c("winpctLag1", "admitRate", "yield", "admissions", "ACTcomposite75", "winpctLag2", "winpctLag3"),
#           title = "Numerical Summary Statistics for BCS Era")
# None of the dummy variables have missing observations.
datasummary(wontitle + top5 + top15 ~ N, data = df.bcs.merged, output = "markdown",
            title = "Categorical Summary Statistics for BCS Era")
datasummary(wontitle + top5 + top15 ~ N, data = df.bcs.merged, output = "latex_tabular",
            title = "Categorical Summary Statistics for BCS Era")

# Now estimate the fixed effects models of interest using the imputed values.

# dependent variable log(applications):
est.bcs.applications <- lm(log(applications) ~ wontitle + wontitleLag1 + wontitleLag2 + wontitleLag3 + top5 + top5Lag1 + top5Lag2 + top5Lag3 + top15 + top15Lag1 + top15Lag2 + top15Lag3 + winpct + winpctLag1 + winpctLag2 + winpctLag3 + log(numHSgrads) + log(medianHHincome) + year + school,
                           data = df.bcs.merged)
clust.applications <- coef_test(est.bcs.applications, vcov = "CR2", cluster = df.bcs.merged$school)

# dependent variable log(enrollments):
est.bcs.enrollments <- lm(log(enrollments) ~ wontitle + wontitleLag1 + wontitleLag2 + wontitleLag3 + top5 + top5Lag1 + top5Lag2 + top5Lag3 + top15 + top15Lag1 + top15Lag2 + top15Lag3 + winpct + winpctLag1 + winpctLag2 + winpctLag3 + log(numHSgrads) + log(medianHHincome) + year + school,
                           data = df.bcs.merged)
clust.enrollments <- coef_test(est.bcs.enrollments, vcov = "CR2", cluster = df.bcs.merged$school)

# dependent variable avgSAT75imputed:
est.bcs.SAT <- lm(avgSAT75imputed ~ wontitle + wontitleLag1 + wontitleLag2 + wontitleLag3 + top5 + top5Lag1 + top5Lag2 + top5Lag3 + top15 + top15Lag1 + top15Lag2 + top15Lag3 + winpct + winpctLag1 + winpctLag2 + winpctLag3 + log(numHSgrads) + log(medianHHincome) + year + school,
                          data = df.bcs.merged)
clust.SAT <- coef_test(est.bcs.SAT, vcov = "CR2", cluster = df.bcs.merged$school)

# display fixed effects regression output for BCS era:
stargazer(est.bcs.applications, est.bcs.enrollments, est.bcs.SAT, 
          se = list(clust.applications$SE, clust.enrollments$SE, clust.SAT$SE),
          p = list(clust.applications$p_Satt, clust.enrollments$p_Satt, clust.SAT$p_Satt),
          type = "text",
          report = "vc*ps",
          title = "BCS Era",
          omit = c("school", "year"),
          notes = "Year and school dummy coefficients are not displayed. Robust standard errors are in parentheses.",
          notes.align = 'l')
stargazer(est.bcs.applications, est.bcs.enrollments, est.bcs.SAT, 
          se = list(clust.applications$SE, clust.enrollments$SE, clust.SAT$SE),
          p = list(clust.applications$p_Satt, clust.enrollments$p_Satt, clust.SAT$p_Satt),
          type = "latex",
          report = "vc*ps",
          title = "BCS Era",
          omit = c("school", "year"),
          notes = "Year and school dummy coefficients are not displayed. Robust standard errors are in parentheses.",
          notes.align = 'l')


