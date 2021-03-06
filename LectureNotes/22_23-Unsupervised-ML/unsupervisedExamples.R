library(magrittr)
library(mclust)
library(mlr)
library(RDRToolbox)

# Set up the data we'll be using
X <- iris[,-5]
Y <- iris[, 5]


#-------------------------------
# k-means using the iris dataset
#-------------------------------
# call the k-means function in base R
clusters <- X %>% kmeans(3)
# equivalently: clusters <- kmeans(X,3)

# print results
print(clusters)

# compare k-means classes with actual classes
print(table(Y,clusters$cluster))
# setosa perfect
# versicolor a bit off
# virginica even more off (hard to predict)

# elbow at k=3

#-------------------------------
# EM using the iris dataset
#-------------------------------
# call the EM clustering function from the mclust package
clusters <- X %>% Mclust(G=3)
# equivalently: clusters <- Mclust(X,G=3)
# 3 points of support for 3 species of flowers

# list inferred probabilities of Pr(Y=y)
# class probabilities (average of posteriors at end of algorithm)
# good since species are 1/3 each (see below)
print(clusters$parameters$pro)

# list frequencies of each species in iris data
print(table(Y))
# 50/150

# list average of X's conditional on being in class y (1, 2, or 3)
print(clusters$parameters$mean)

# list posterior class probabilities for each observation in our training data
# first observations are setosa, so probability of that class (1) should be high
head(clusters$z)

# compare EM classes with actual classes
# setosa did well
# versicolor messed up a bit
# virginica did well
print(table(Y,clusters$classification))


#-------------------------------
# Principal components analysis
#-------------------------------
# computing eigen values 
# eigen values tell us how the data needs to be transformed in order to preserve original variation in the data after removing some vars.
cormat <- X %>% cor(.)
# correlation matrix
print(cormat)
# eigen values; only unique when matrix is full rank (no perfect collinearity among X's)
# one of the eigen values will be zero if matrix not full rank.
# larger eigen value --> more variance
print(eigen(cormat)$values)

# one vector for every eigen value (4 eigen values --> 4 eigen vectors)
# compute eigen vectors; transformation of correlation matrix.
v <- eigen(cormat)$vectors
# v transpose times v
print(crossprod(v)) # should be identity matrix

# do PCA using base R function prcomp
prin.comp <- prcomp(X, scale. = T)

# show eigen vectors (will be same as v)
print(prin.comp$rotation)

# show eigen values; standard deviations of object squared
print(prin.comp$sdev^2)

# which components explain the most variance?
# first component gives 73% of the data variation
# first and second component give ~96% of variation in data; don't really need the others for example...
prop.var.explained <- prin.comp$sdev^2/sum(prin.comp$sdev^2)
print(prop.var.explained)

# keep only the first three components (since these explain 99.5% of the variance)
reducedX <- prin.comp$x#[,seq(1,2)]


## try EM clustering on reduced data ... can we still get the same answer?
#-------------------------------
clustersDR <- reducedX %>% Mclust(G=3)
# equivalently: clusters <- Mclust(X,G=3)

# list inferred probabilities of Pr(Y=y)
print(clustersDR$parameters$pro)

# list frequencies of each species in iris data
print(table(Y))

# list average of X's conditional on being in class y
print(clustersDR$parameters$mean)

# list posterior class probabilities for each observation in our training data
head(clustersDR$z)

# compare EM classes with actual classes
print(table(Y,clustersDR$classification))


## try k-means on reduced data ... how does it perform?
#-------------------------------
clustersKDR <- reducedX %>% kmeans(3)
# equivalently: clusters <- kmeans(X,3)

# print results
print(clustersKDR)

# compare k-means classes with actual classes
print(table(Y,clustersKDR$cluster))


## try naive Bayes in full and reduced data ... how do we do?
#-------------------------------
# Full data
n <- nrow(iris)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
iris.train <- iris[train,]
iris.test  <- iris[test, ]

# classification task
classifTask <- makeClassifTask(data = iris.train, target = "Species")

# Set prediction algorithm
alg.nb    <- makeLearner("classif.naiveBayes", predict.type = "response")

# Train the model
final.nb    <- train(learner = alg.nb,    task = classifTask)

# Predict in test set
pred.nb    <- predict(final.nb,    newdata = iris.test)

# Assess performance
print(performance(pred.nb,    measures = list(kappa)))



# Reduced data
reducedIris <- as.data.frame(cbind(iris[,5],reducedX))
colnames(reducedIris)[1] <- "Species"
reducedIris$Species <- as.factor(reducedIris$Species)

n <- nrow(iris)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
iris.train <- reducedIris[train,]
iris.test  <- reducedIris[test, ]

# classification task
classifTask <- makeClassifTask(data = iris.train, target = "Species")

# Set prediction algorithm
alg.nb    <- makeLearner("classif.naiveBayes", predict.type = "response")

# Train the model
final.nb    <- train(learner = alg.nb,    task = classifTask)

# Predict in test set
pred.nb    <- predict(final.nb,    newdata = iris.test)

# Assess performance
print(performance(pred.nb,    measures = list(kappa)))


#-------------------------------
# Nonlinear Dimension Reduction
#-------------------------------
# Use RDRToolbox package
isomapped <- Isomap(data=as.matrix(X), dims=3, k=5)
XreducedNL <- isomapped$dim3

## try EM clustering on reduced data ... can we still get the same answer?
#-------------------------------
clustersNLDR <- XreducedNL %>% Mclust(G=3)
# equivalently: clusters <- Mclust(X,G=3)

# list inferred probabilities of Pr(Y=y)
print(clustersNLDR$parameters$pro)

# list frequencies of each species in iris data
print(table(Y))

# list average of X's conditional on being in class y
print(clustersNLDR$parameters$mean)

# list posterior class probabilities for each observation in our training data
head(clustersNLDR$z)

# compare EM classes with actual classes
print(table(Y,clustersNLDR$classification))


