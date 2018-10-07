suppressPackageStartupMessages(library(tree))
suppressMessages(library(ISLR))
attach(Carseats)

############################ CLASSIFICATION TREE ###############################

# create binary 'high' variable
High <- ifelse(Carseats$Sales <= 8, 'no','yes')
Carseats <- data.frame(Carseats, High)

# classification decision tree
tree.carseats <- tree(High ~ . - Sales, data = Carseats)
summary(tree.carseats)

# plot decision tree
plot(tree.carseats)
text(tree.carseats, pretty = 0)

# validation set approach
set.seed(5)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ . - Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats, newdata = Carseats.test, type = 'class')
confusion_matrix <- table(tree.pred, High.test)
confusion_matrix
(confusion_matrix[1,1] + confusion_matrix[2,2]) / 200

# cross validation
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

# plot of cross validation
par(mfrow = c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type = 'b')
plot(cv.carseats$k, cv.carseats$dev, type = 'b')

# pruned tree
prune.carseats <- prune.misclass(tree.carseats, best = 9)
par(mfrow = c(1,1))
plot(prune.carseats)
text(prune.carseats, pretty = 0)

# prediction using the pruned tree
tree.pred <- predict(prune.carseats, newdata = Carseats.test, type = 'class')
confusion_matrix <- table(tree.pred, High.test)
confusion_matrix

# correct classification rate
(confusion_matrix[1,1] + confusion_matrix[2,2]) / 200

# prediction using pruned tree (15)
prune.carseats <- prune.misclass(tree.carseats, best = 15)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
tree.pred <- predict(prune.carseats, newdata = Carseats.test, type = 'class')
table(tree.pred, High.test)
(table(tree.pred, High.test)[1,1] + table(tree.pred, High.test)[2,2]) / 200

############################ REGRESSION TREE ###################################

suppressPackageStartupMessages(library(MASS))
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., Boston, subset = train)
summary(tree.boston)

# plots
plot(tree.boston)
text(tree.boston, pretty = 0)

# cross validation
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

# prune tree
prune.boston <- prune.tree(tree.boston, best = 5)

# plot pruned tree
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, 'medv']
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)

######################### BAGGING AND RANDOM FOREST ############################

suppressPackageStartupMessages(library(randomForest))
set.seed(1)
bag.boston <- randomForest(medv ~ .,
                           data = Boston,
                           subset = train,
                           mtry = 13,
                           importance = TRUE)
bag.boston
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

# bagged decision tree with number of trees equal to 25
bag.boston <- randomForest(medv ~ .,
                           data = Boston,
                           subset = train,
                           mtry = 13,
                           ntree = 25,
                           importance = TRUE)
yhat.bag <- predict(bag.boston, newdata = Boston[-train,])
mean((yhat.bag - boston.test)^2)

# random forest
set.seed(1)
rf.boston <- randomForest(medv ~ .,
                          data = Boston,
                          subset = train,
                          mtry = 6,
                          importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train,])
mean((yhat.rf - boston.test)^2)

# feature importances
importance(rf.boston)
varImpPlot(rf.boston)

################################## BOOSTING ####################################

suppressPackageStartupMessages(library(gbm))
set.seed(1)
boost.boston <- gbm(medv ~ .,
                    data = Boston[train,],
                    distribution = 'gaussian',
                    n.trees = 5000,
                    interaction.depth = 4)
summary(boost.boston)

# partial dependence plots
par(mfrow = c(1,2))
plot(boost.boston, i = 'rm')
plot(boost.boston, i = 'lstat')

yhat.boost <- predict(boost.boston,
                      newdata = Boston[-train,],
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2)

# modify lambda, shrinkage parameter
boost.boston <- gbm(medv ~ .,
                    data = Boston[train,],
                    distribution = 'gaussian',
                    n.trees = 5000,
                    interaction.depth = 4,
                    shrinkage = 0.2,
                    verbose = F)
yhat.boost <- predict(boost.boston,
                      newdata = Boston[-train,],
                      n.trees = 5000)
mean((yhat.boost - boston.test)^2)
