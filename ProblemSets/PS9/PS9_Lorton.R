# ECON 5253: PS9 R Script
# William Lorton
# April 13 2021

library(glmnet)
library(tidymodels)
library(magrittr)
library(tidyverse)

# Q4:

# Loading the housing data from UCI.
# read_table for reading white space delimited data in
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
# add column names since they are missing
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
# See example script from lecture for variable description (or website).

# Q5:
# Set seed to 123456.
# Makes it so results involving random number generation are reproducible across machines (like turning to a
# specific page of the famous random numbers book).
set.seed(123456)

# Q6:
# Create two data sets called housing_train and housing_test using the initial_split() function from the
# rsample package.
# prop gives proportion of training data (80% of data here)
housing_split <- initial_split(housing, prop = 0.8)
# training data
housing_train <- training(housing_split)
# testing data
housing_test  <- testing(housing_split)

# Q7:
# Create a new recipe() that takes the log of the housing value, converts chas to a factor, creates
# 6th degree polynomials of each of the continuous features (all vars apart from chas), and linear 
# interactions of each.
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,lstat,dis,nox, degree=6)

# Run the recipe
housing_prep          <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped  <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x  <- housing_test_prepped  %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select( medv)
housing_test_y  <- housing_test_prepped  %>% select( medv)

# dimension of the training data (housing_train)
# 405 rows, 14 columns
dim(housing_train)

# How many more X variables do we have in the original housing data compared to after
# pre-processing the data with the recipe above?
# Started with 13 X variables (don't count the y medv).
dim(housing)
# Have 74 X variables after pre-processing. 
dim(housing_train_x)

# Q8:
# Estimate a LASSO model to predict log median house value.
# Penalty parameter lambda is tuned by 6-fold cross validation.
tune_spec <- linear_reg(
  # tuning parameter
  penalty = tune(),
  # 1 = LASSO, 0 = ridge
  mixture = 1       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda; how refined will lambda be (resolution of it)?
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold (k=6) cross-validation; re-sample within training data 10 different times; v (k) should be 3 to 10.
rec_folds <- vfold_cv(housing_train_x %>% bind_cols(tibble(medv = housing_train_y$medv)), v = 6)

# Workflow package to do k-fold cross-validation
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# optimal lambda value
top_rmse  <- show_best(rec_res, metric = "rmse")
top_rmse
best_rmse <- select_best(rec_res, metric = "rmse")
# optimal lambda value is 0.00139
# in-sample RMSE is 0.169
best_rmse

# Re-do to get out-of-sample RMSE (use test data instead of train data).

tune_spec <- linear_reg(
  # tuning parameter
  penalty = tune(),
  # 1 = LASSO, 0 = ridge
  mixture = 1       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda; how refined will lambda be (resolution of it)?
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold (k=6) cross-validation; re-sample within training data 10 different times; v (k) should be 3 to 10.
rec_folds <- vfold_cv(housing_test_x %>% bind_cols(tibble(medv = housing_test_y$medv)), v = 6)

# Workflow package to do k-fold cross-validation
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# optimal lambda value
top_rmse  <- show_best(rec_res, metric = "rmse")
top_rmse
best_rmse <- select_best(rec_res, metric = "rmse")
# optimal lambda value is 0.00356
# out-of-sample RMSE is 0.181
best_rmse

# Q9:
# Estimate a ridge regression model to predict log median house value.
# Penalty parameter lambda is tuned by 6-fold cross validation.

tune_spec <- linear_reg(
  # tuning parameter
  penalty = tune(),
  # 1 = LASSO, 0 = ridge
  mixture = 0       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda; how refined will lambda be (resolution of it)?
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold (k=6) cross-validation; re-sample within training data 10 different times; v (k) should be 3 to 10.
rec_folds <- vfold_cv(housing_train_x %>% bind_cols(tibble(medv = housing_train_y$medv)), v = 6)

# Workflow package to do k-fold cross-validation
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# optimal lambda value
top_rmse  <- show_best(rec_res, metric = "rmse")
top_rmse
best_rmse <- select_best(rec_res, metric = "rmse")
# optimal lambda value is 0.0373
# in-sample RMSE is 0.176
best_rmse

# Re-do to get out-of-sample RMSE (use test data instead of train data).

tune_spec <- linear_reg(
  # tuning parameter
  penalty = tune(),
  # 1 = LASSO, 0 = ridge
  mixture = 0       
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda; how refined will lambda be (resolution of it)?
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold (k=6) cross-validation; re-sample within training data 10 different times; v (k) should be 3 to 10.
rec_folds <- vfold_cv(housing_test_x %>% bind_cols(tibble(medv = housing_test_y$medv)), v = 6)

# Workflow package to do k-fold cross-validation
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(medv ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# optimal lambda value
top_rmse  <- show_best(rec_res, metric = "rmse")
top_rmse
best_rmse <- select_best(rec_res, metric = "rmse")
# optimal lambda value is 1.00e-10
# out-of-sample RMSE is 0.197
best_rmse

# Q10:
# tex file stuff.

# Would you be able to estimate a SLR model on a data set that had more columns than rows?
# No. We would have more variables and therefore slope parameters to estimate than we'd have data.
# It would be impossible to even get a full set of unique OLS coefficient estimates since we would run into
# a perfect multicollinearity problem. Also, we can see from the formula for the variance of the error
# term (the error variance) that the value would become negative should we have more variables than
# observations; this clearly doesn't make any sense (and the RMSE would be undefined obviously as well).

# Using the RMSE values of each of the tuned models in the previous two questions, comment on where your
# model stands in terms of the bias-variance trade-off.




