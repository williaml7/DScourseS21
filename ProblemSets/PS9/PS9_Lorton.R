library(tidyverse)
library(tidymodels)
library(magrittr)

set.seed(123456)

housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's

# Q6:
# Create training and test data sets.
# 80% of data will be for training.
# 20% of data for test.
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

#####################
# Step 1: Add new features
#####################
# Q7:
# use recipe function to pre-process the data.
housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
                  # convert outcome variable to logs
                  step_log(all_outcomes()) %>%
                  # convert 0/1 chas to a factor
                  step_bin2factor(chas) %>%
                  # create interaction term between crime and nox
                  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
                  # create square terms of some continuous variables
                  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,lstat,dis,nox, degree=6) %>%
                  # prep
                  prep()

# Run the recipe
housing_train_prepped <- housing_recipe %>% juice
housing_test_prepped  <- housing_recipe %>% bake(new_data = housing_test)

# Dimension of original data:
# 1 y variable (medv) and 13 x variables to make 14 columns
dim(housing) 
# Dimension of training data:
# 1 y variable (medv) and 74 x variables to make 75 columns
dim(housing_train_prepped)
# We have 61 more x variables going from original data to prepped training data (leaving medv
# to be the dependent variable instead of an "x").

# Q8:
#####################
# Start with LASSO
#####################
# set up the task and the engine
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(log(medv) ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Train with tuned lambda.
final_lasso <- finalize_workflow(rec_wf,
                                 best_rmse
                                )
# Show results in test set.
last_fit(final_lasso, housing_split) %>%
         collect_metrics() %>% print
# Out-of-sample RMSE of 0.220

top_rmse %>% print(n = 1)
# Optimal lambda value of 0.00222
# in-sample RMSE of 0.0625

# Q9:
#####################
# Now do Ridge
#####################
# set up the task and the engine
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of the regularization parameter lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_model(tune_spec) %>%
  add_formula(log(medv) ~ .)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# what is the best value of lambda?
top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

final_ridge <- finalize_workflow(rec_wf,
                                 best_rmse
                                )
last_fit(final_ridge,housing_split) %>%
         collect_metrics() %>% print
# out-of-sample RMSE of 0.219

top_rmse %>% print(n = 1)
# Optimal lambda value of 0.0373
# in-sample RMSE of 0.0694
