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
confusionMatrix(reference = y.hat,
                data = dat[-train, 'y'])$table

################################################################################
################################### ROC CURVES #################################
################################################################################

# Deviated from lab to account for inability to install ROCR: using ggplot2 

# model 
svm.opt <- svm(y ~ .,
               data = dat[train, ],
               kernel = 'radial',
               gamma = 2,
               cost = 1,
               probability = TRUE) 

y.hat.prob <- attr(predict(svm.opt,
                      newdata = dat[train, ],
                      probability = TRUE), "probabilities")

# get true and false positive rates for varying thresholds
thresholds <- seq(0.01, 1, 0.01)
tp.rate <- rep(0, length(thresholds))
fp.rate <- rep(0, length(thresholds))

for (i in 1:length(thresholds)) {
    y.hat <- ifelse(y.hat.prob[, 1] > thresholds[i], '2', '1')
    cm <- confusionMatrix(data = factor(y.hat, levels = 1:2),
                          reference = factor(dat[train, 'y'], levels = 1:2))
    tp.rate[i] <- cm$byClass['Sensitivity']
    fp.rate[i] <- 1 - cm$byClass['Specificity']
}

roc_df <- data.frame(x = fp.rate, y = tp.rate)

# roc plot
ggplot(roc_df, aes(x, y)) +
    geom_line(color = 'blue') +
    ggtitle("Support Vector Machine Training ROC",
            "Positive class = 2") +
    xlab("False Positive Rate (1 - Specificity)") +
    ylab("True Positive Rate (Sensitivity)")


# increase gamma
svm.opt <- svm(y ~ .,
               data = dat[train, ],
               kernel = 'radial',
               gamma = 50,
               cost = 1,
               probability = TRUE) 

y.hat.prob <- attr(predict(svm.opt,
                           newdata = dat[train, ],
                           probability = TRUE), "probabilities")

# get true and false positive rates for varying thresholds
thresholds <- seq(0.01, 1, 0.01)
tp.rate <- rep(0, length(thresholds))
fp.rate <- rep(0, length(thresholds))

for (i in 1:length(thresholds)) {
    y.hat <- ifelse(y.hat.prob[, 1] > thresholds[i], '2', '1')
    cm <- confusionMatrix(data = factor(y.hat, levels = 1:2),
                          reference = factor(dat[train, 'y'], levels = 1:2))
    tp.rate[i] <- cm$byClass['Sensitivity']
    fp.rate[i] <- 1 - cm$byClass['Specificity']
}

roc_df <- data.frame(x = fp.rate, y = tp.rate)

# roc plot
ggplot(roc_df, aes(x, y)) +
    geom_line(color = 'blue') +
    ggtitle("Support Vector Machine Training ROC",
            "Positive class = 2") +
    xlab("False Positive Rate (1 - Specificity)") +
    ylab("True Positive Rate (Sensitivity)")