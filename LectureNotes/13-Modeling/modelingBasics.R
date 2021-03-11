library(tidyverse)

# Use the motor trend cars data set
df <- as_tibble(mtcars)
df
# note that all vars are dbl --> R treating as real number
# but a number of vars should be factor instead (e.g. vs).


# mpg = b0 + b1*hp + b2*wt
# mpg = miles/gallon
# hp = horse power
# wt = weight (1000lbs)
# linear regression (OLS)
est <- lm(mpg ~ hp + wt, data=df)
summary(est)

# plot regression line
ggplot(df, aes(hp,mpg)) +
    geom_point() +
    geom_smooth(method='lm')

## Prediction problem
# what is mpg of a car with 3000 lbs (wt = 3) and 160 hp (hp = 160)?
mpg_hat <- 37.22727 - 0.03177*160 - 3.87783*3
mpg_hat

## Causal inference problem
# what would be change in mpg of car be if I reduced wt by 500 lbs?
mpg_diff <- -3.87783*(-.5)
mpg_diff

# New model: V engine or Straight engine?
# dependent var is categorical (v or straight engine);
# 0 = v
# 1 = straight
# generalized linear model; non-linear model.
est <- glm(vs ~ hp + wt, data=df, family = "binomial")
est1<- glm(as.factor(vs) ~ hp + wt, data=df, family = "binomial") # same thing
summary(est)
summary(est1)

## Prediction problem: what is Pr(vs=1) if hp = 160, wt=3000?
# sigmoid function;
p <- exp(7.41037 - 0.08535*160 + 1.00334*3)/
    (1+exp(7.41037 - 0.08535*160 + 1.00334*3))
p
# 3.8% chance of vs = 1 with these values of hp and wt.

## R does it for us; prediction function automatically (uses sigmoid automatically).
newdf <- df %>% mutate(hp=160,wt=3) %>% slice(1) # first create a data frame at the values of X that we want
newdf
zero.one.r <- predict(est, newdata = newdf) # to output the inside of the sigmoid function
zero.one.r # x beta hat
prob.r <- predict(est, newdata = newdf, type = "response") # to output a probability
prob.r # 3.8% chance of vs = 1 with these values of hp and wt (as before).
