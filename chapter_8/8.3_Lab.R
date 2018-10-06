suppressPackageStartupMessages(library(tree))
suppressMessages(library(ISLR))
attach(Carseats)

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
