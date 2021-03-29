library(nloptr)

#----------------------------------------------------
# Basic gradient descent manually (no package usage)
#----------------------------------------------------

# set up a stepsize
alpha <- 0.003
# set up a number of iteration
iter <- 500
# define the gradient (first derivative) of f(x) = x^4 - 3*x^3 + 2
gradient <- function(x) return((4*x^3) - (9*x^2))
# randomly initialize a value to x
set.seed(100)
x <- floor(runif(1)*10)
# create a vector to contain all xs for all steps
x.All <- vector("numeric",iter)
# gradient descent method to find the minimum
for(i in 1:iter){
    x <- x - alpha*gradient(x)
    x.All[i] <- x
    print(x)
}
# print result and plot all xs for every iteration
print(paste("The minimum of f(x) is ", x, sep = ""))


#----------------------------------------------------
# Basic gradient descent using nloptr package
#----------------------------------------------------

# Our objective function
eval_f <- function(x) {
    return( x[1]^4 - 3*x[1]^3 + 2 )
}
# Gradient of our objective function
eval_grad_f <- function(x) {
    return( 4*x[1]^3 - 9*x[1]^2 )
}
# initial values
x0 <- -5
# Algorithm parameters
# using L-BFGS
# tolerance of 10^-8; stop when difference between new x and previous x is this or less.
opts <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8)
# Find the optimum!
res <- nloptr( x0=x0, eval_f=eval_f, eval_grad_f=eval_grad_f, opts=opts)
print(res)

#----------------------------------------------------
# Nelder-Mead method using nloptr
#----------------------------------------------------

# Our objective function
objfun <- function(x) {
    return( x[1]^4 - 3*x[1]^3 + 2 )
}
# initial values
xstart <- 5
# Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-8)
# Find the optimum!
res <- nloptr( x0=xstart,eval_f=objfun,opts=options)
print(res)

#----------------------------------
# OLS estimation with L-BFGS
#----------------------------------

## Our objective function
objfun <- function(beta,y,X) {
return (sum((y-X%*%beta)^2))
# equivalently, if we want to use matrix algebra:
# return ( crossprod(y-X%*%beta) )
}

## Gradient of our objective function
gradient <- function(beta,y,X) {
return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## read in the data
# Predict sepal length with sepal width, petal length, petal width, and species
# using data from the iris data set
y <- iris$Sepal.Length
# X is matrix of the covariate data (each column is an independent variable).
# first column is filled with 1s for the intercept of the model.
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
# There are six beta parameters to estimate.
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)
# optimal value of objective function gives the minimum value of the sum of squared residuals.
# optimal value of controls should give the parameter estimates that OLS produces (see below).

## Check solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))

#----------------------------------
# MLE estimation with Nelder Mead
#----------------------------------

## Our objective function
objfun  <- function(theta,y,X) {
# need to slice our parameter vector into beta and sigma components
beta    <- theta[1:(length(theta)-1)]
sig     <- theta[length(theta)]
# write objective function as *negative* log likelihood (since NLOPT minimizes)
loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
return (loglike)
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris))$coefficients[,1]),runif(1))

## Algorithm parameters
# Using Nelder-Mead
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr(x0=theta0, eval_f=objfun, opts=options, y=y, X=X)
print(result)
# optimal beta coefficient estimates.
betahat  <- result$solution[1:(length(result$solution)-1)]
# Residual standard error: standard deviation of the error term; sqrt(RSS/(n-2))
sigmahat <- result$solution[length(result$solution)]

## Check solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))

#----------------------------------
# MLE estimation with L-BFGS
#----------------------------------
## Our objective function
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
grad[length(theta)]       <- dim(X)[1]/sig-crossprod (y-X%*%beta)/(sig^3)
return ( grad )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients
theta0 <- append(as.vector(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris))$coefficients[,1]),runif(1))

## Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,eval_grad_f=gradient,opts=options,y=y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]

## Check solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))


#----------------------------------
# Batch gradient descent with OLS
#----------------------------------

# set up a stepsize
alpha <- 0.00003

# set up a number of iterations
maxiter <- 500000

## Our objective function
objfun <- function(beta,y,X) {
return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,y,X) {
return ( as.vector(-2*t(X)%*%(y-X%*%beta)) )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta),maxiter)

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

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

## Closed-form solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))

#----------------------------------
# Stochastic gradient descent with OLS
#----------------------------------

# set up a stepsize
alpha <- 0.0003

# set up a number of iterations
maxiter <- 500000

## Our objective function
objfun <- function(beta,y,X) {
return ( sum((y-X%*%beta)^2) )
}

# define the gradient of our objective function
gradient <- function(beta,y,X) {
return ( as.vector(-2*X%*%(y-t(X)%*%beta)) )
}

## read in the data
y <- iris$Sepal.Length
X <- model.matrix(~Sepal.Width+Petal.Length+Petal.Width+Species,iris)

## initial values
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)

# create a vector to contain all beta's for all steps
beta.All <- matrix("numeric",length(beta), maxiter)

# stochastic gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-12) {
    # Randomly re-order the data (gets indexes for rows from covariate data matrix X)
    random <- sample(nrow(X))
    # use index to get actual values along rows of X
    X <- X[random,]
    y <- y[random]
    # Update parameters for each row of data (150 rows)
    for(i in 1:dim(X)[1]){
        beta0 <- beta
        beta <- beta0 - alpha*gradient(beta0,y[i],as.matrix(X[i,]))
        beta.All[,i] <- beta
    }
    alpha <- alpha/1.0005
    if (iter%%1000==0) {
        print(beta)
    }
    iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))

## Closed-form solution
print(summary(lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width+Species,data=iris)))

#----------------------------------
# Cost functions note
#----------------------------------

# Linear regression (OLS) cost function:
objfun <- sum((y-X%*%beta)^2) 
# matrix form:
objfun <- crossprod(y-X%*%beta)
# if normal MLE:
loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) )

# Logistic regression (foundation of ANN):
loglike <- -sum( (y==1)*(log(h(X%*%beta))) + (y==0)*(log(1-h(X%*%beta))) )
#where; h() is the sigmoid or logistic function.
h(X%*%beta) = exp(X%*%beta)/(1+exp(X%*%beta))

# Support Vector Machine (SVM):
# similar to least squares, but does not count errors that are sufficiently small.
costfun <- sum(max(0,1-y*(w%*%X))) + lambda/(sum(w^2))
# w is the support vector which divides the sample space.

