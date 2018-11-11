suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(caret))

################################################################################
############################ SUPPORT VECTOR CLASSIFIER #########################
################################################################################

set.seed(1)

# generate data
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1, ] = x[y==1, ] + 1

# plot data
qplot(x[, 1], x[, 2], color = y)

dat <- data.frame(x = x, y = as.factor(y))    
svmfit <- svm(y ~ .,
              data = dat,
              kernel = 'linear',
              cost = 10,
              scale = FALSE)

# plot model
plot(svmfit, dat)

# indices of support vectors
svmfit$index

summary(svmfit)

# adjust cost parameter
svmfit <- svm(y ~ .,
              data = dat,
              kernel = "linear",
              cost = 0.1,
              scale = FALSE)
plot(svmfit, dat)

# cross validation for range of cost argument (C parameter)
set.seed(1)
tune.out <- tune(svm,
                 y ~ .,
                 data = dat,
                 kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1.5, 10, 100)))
summary(tune.out)

best.model <- tune.out$best.model
summary(best.model)

# create test data
x.test <- matrix(rnorm(20*2), ncol = 2)
y.test <- sample(c(-1, 1), 20, rep = TRUE)
x.test[y.test == 1,] = x.test[y.test == 1, ] + 1
testdat <- data.frame(x = x.test, y = as.factor(y.test))

# predict with best model
y.hat <- predict(best.model, testdat)
confusionMatrix(data = as.factor(y.hat),
                reference = testdat$y)$table

svmfit <- svm(y ~ .,
              data = dat,
              kernel = "linear",
              cost = 0.01,
              scale=FALSE)

y.hat <- predict(svmfit, testdat)
confusionMatrix(data = as.factor(y.hat),
                reference = testdat$y)$table

# linearly separable case
x[y==1, ] = x[y==1, ] + 0.5
qplot(x[, 1], x[, 2], color = y)

dat <- data.frame(x = x, y = as.factor(y))

svmfit <- svm(y ~ .,
              data = dat,
              kernel = "linear",
              cost = 1e5)
summary(svmfit)

plot(svmfit, dat)

# smaller cost for violations to margin/hyperplane
svmfit <- svm(y ~ .,
              data = dat,
              kernel = "linear",
              cost = 1)
summary(svmfit)

plot(svmfit, dat)

################################################################################
############################ SUPPORT VECTOR MACHINE ############################
################################################################################

set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ] = x[1:100,] + 2
x[101:150, ] = x[101:150, ] - 2

y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))

# plot data
qplot(x[, 1], x[, 2], color = y)

# train test split

train <- sample(200, 100)
svmfit <- svm(y ~ .,
              data = dat[train, ],
              kernel = 'radial',
              gamma = 1,
              cost = 1)

# plot model
plot(svmfit, dat[train, ])

summary(svmfit)

# try lower tolerance for violations to the margin/hyperplane
svmfit <- svm(y ~ .,
              data = dat[train, ],
              kernel = 'radial',
              gamma = 1,
              cost = 1e5)

# plot model
plot(svmfit, dat[train, ])

# cross validation for non-linear SVM
tune.out <- tune(svm,
                 y ~ .,
                 data = dat[train, ],
                 kernel = 'radial',
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# predictions
y.hat <- predict(tune.out$best.model, newdata = dat[-train, ])
