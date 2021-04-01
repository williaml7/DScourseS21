# ECON 5253: PS8 R script

library(nloptr)
library(modelsummary)

###############################################################################################

# set seed of pseudo-random number generator
set.seed(100)

# Create data set

# X is a matrix of dimension N = 100,000 by K = 10 containing normally distributed 
# random numbers, except the first column which should be a column of 1's.

# Epsilon is a vector of length N containing random numbers ~ N(0, sigma^2) where
# sigma = 0.5 --> sigma^2 = 0.25.

# Beta is a vector of length 10 with values as desired in the PS8 instructions.

N <- 100000
K <- 10
sigma <- 0.5

# Make X matrix.
X <- matrix(data = rnorm(n = N*K, mean = 0, sd = sigma), nrow = N, ncol = K)
X
dim(X)

# Fill first column with ones.
X[,1] <- 1
X[,1]
X

# Make epsilon.
eps <- rnorm(n = N, mean = 0, sd = sigma)
length(eps)

# Make beta.
beta.true <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)
beta.true

# Y is a vector equal to X*beta + epsilon; should be a N (100,000) by 1 vector.
Y <- X%*%beta.true + eps
length(Y)

###############################################################################################

# Use the matrices to compute OLS estimate of the beta vector using the closed-form solution:
# Should result in a K by 1 vector of beta estimates.
beta.hat <- solve((t(X)%*%X))%*%t(X)%*%Y
beta.hat
# Check that it's exactly the same as:
summary(lm(Y~X -1))

# Showing the difference between beta.hat and true value vector (beta):
abs(beta.true - beta.hat)
# They are clearly all quite close.

###############################################################################################

# Compute beta OLS estimate using (batch) gradient descent:

# Set "learning rate" or step size.
alpha <- 0.0000003

# set up a number of iterations.
maxiter <- 500000

## Objective function (minimize residual sum of squares).
objfun <- function(beta,y,X) {
  return ( sum((y-X%*%beta)^2) )
}

# define the gradient (derivative) of our objective function.
gradient <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)))
}

## read in the data
y <- Y
X <- X

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric", length(beta), maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
# loop exits once condition is false (difference between new guesses is very small).
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print number of iterations it took to hit accuracy requirement
print(iter)
# print estimates
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))
beta.gradientDescent <- beta
beta.gradientDescent
# Look fine relative to:
beta.true

###############################################################################################

# Use nloptr's L-BFGS algorithm to get beta estimates:

# Our objective function (minimize residual sum of squares).
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

## Gradient (derivative) of our objective function
gradient <- function(beta,y,X) {
  return (as.vector(-2*t(X)%*%(y-X%*%beta)))
}

# Read in the data
y <- Y
X <- X

## initial values
# There are 10 beta parameters to estimate.
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
beta0

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-6, "maxeval"=1e3)

## Optimize.
result.lbfgs <- nloptr(x0=beta0, eval_f=objfun, eval_grad_f=gradient, opts=options, y=y, X=X)
print(result.lbfgs)
# optimal value of objective function gives the minimum value of the sum of squared residuals.
# optimal value of controls should give the parameter estimates that OLS produces.
# These look fine.
beta.true

###############################################################################################

# Using nloptr's Nelder-Mead (simplex) algorithm to get beta estimates: 

# Remember that it only needs an objective function; no derivative is taken (it uses a 
# transformable triangle to help find the optimal values).

# Our objective function (minimize residual sum of squares).
objfun <- function(beta,y,X) {
  return (sum((y-X%*%beta)^2))
}

# Read in the data
y <- Y
X <- X

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients (10).

# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD", "xtol_rel"=1.0e-8, "maxeval"=1e3)
# Optimize.
result.nelderMead <- nloptr(x0=beta0, eval_f=objfun, opts=options, y=y, X=X)
print(result.nelderMead)
# Look fine again.
beta.true

# Do the Nelder-Mead and L-BFGS estimates differ?
# They do, but only very slightly.
result.nelderMead
result.lbfgs

###############################################################################################

# Compute maximum likelihood estimate (MLE) for beta via nloptr's L-BFGS algorithm.

# Our objective function (maximize likelihood function or minimize its negative)
objfun  <- function(theta,y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

## Gradient of the objective function
gradient <- function (theta,y,X) {
  grad     <- as.vector(rep(0,length(theta)))
  beta     <- theta [1:(length(theta)-1)]
  sig      <- theta [length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(y - X%*%beta)/(sig^2)
  grad[length(theta)]       <- dim(X)[1]/sig-crossprod(y-X%*%beta)/(sig^3)
  return ( grad )
}

## read in the data
y <- Y
X <- X

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS", "xtol_rel"=1.0e-6, "maxeval"=1e4)

## Optimize!
result <- nloptr(x0=theta0, eval_f=objfun, eval_grad_f=gradient, opts=options, y=y, X=X)
print(result)
beta.lbfgsMLE  <- result$solution[1:(length(result$solution)-1)]
beta.lbfgsMLE
sigma.lbfgsMLE <- result$solution[length(result$solution)]
sigma.lbfgsMLE

###############################################################################################

# Compute beta estimate using lm() function:
beta.OLS <- lm(Y ~ X -1)
summary(beta.OLS)
# Note that we already found this in our closed-form solution earlier in the script.
beta.hat
# Similar to
beta.true

modelsummary(beta.OLS, output = "markdown")
modelsummary(beta.OLS, output = "latex")



