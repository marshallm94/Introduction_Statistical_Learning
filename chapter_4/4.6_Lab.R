install.packages("ISLR")
install.packages("MASS")
library(ISLR)
library(MASS)
library(class)
names(Smarket)
head(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Year, Volume)

# Logistic Regression
fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial)
summary(fit)
summary(fit)$coef
contrasts(Direction)
probs <- predict(fit, type = "response")
predictions <- rep("Down", 1250)
predictions[probs > 0.5] = "Up"
cm <- table(predictions, Direction)
accuracy <- function(cm){
    accuracy <- sum(cm[1,1], cm[2,2]) / sum(cm)
    accuracy
}
precision <- function(cm) {
    precision <- cm[2,2] / sum(cm[2,])
    precision
}
recall <- function(cm) {
    recall <- cm[2,2] / sum(cm[,2])
    recall
}
accuracy(cm)
precision(cm)
recall(cm)

# Train-Test Split
train <- (Year < 2005)
Smarket.2005 <-  Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <-  Direction[!train]
train_fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
           family = binomial, subset=train)
test_probs <- predict(train_fit, Smarket.2005, type='response')
test_predictions <- rep("Down", 252)
test_predictions[test_probs > 0.5] = "Up"
test_cm <- table(test_predictions, Direction.2005)
accuracy(test_cm)
precision(test_cm)
recall(test_cm)

# 2 predictor fit
lag_1_and_2_fit <- glm(Direction ~ Lag1 + Lag2, data=Smarket,
                       family=binomial, subset = train)
lag_1_2_probs <- predict(lag_1_and_2_fit, Smarket.2005, type="response")
lag_1_2_predictions <- rep("Down", 252)
lag_1_2_predictions[lag_1_2_probs > 0.5] <- "Up"
lag_1_2_cm <- table(lag_1_2_predictions, Direction.2005)
accuracy(lag_1_2_cm)
precision(lag_1_2_cm)
recall(lag_1_2_cm)
predict(lag_1_and_2_fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)),
        type = 'response')

# Linear Discriminant Analysis
mod_lda <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda_predictions <- predict(mod_lda, Smarket.2005)
names(lda_predictions)
# note that the number of columns in the posterior will equal the number of classes
# possible, and the sum of all the probabilities will be equal to one
lda_predictions_class = lda_predictions$class
lda_cm <- table(lda_predictions_class, Direction.2005)
lda_cm == lag_1_2_cm
# note that the model produced by linear discriminant analysis and that produced
# by logistic regression are equal
accuracy(lda_cm)

# Quadratic Discriminant Analysis
mod_qda <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda_class <- predict(mod_qda, Smarket.2005)$class
qda_cm <- table(qda_class, Direction.2005)
accuracy(qda_cm)
# Since the quadratic discriminant analysis has a higher accuracy than both of 
# the other two models (linear discriminant analysis and logistic regression), it 
# is reasonable to assume that the true decision boundary is non-linear (although
# a hypothesis would be necessary to prove a statistically significant difference
# between the accuracy of the two models)
 
# KNN
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]
set.seed(1)
knn_predictions <- knn(train.X, test.X, train.Direction, k=3)
knn_cm <- table(knn_predictions, Direction.2005)
accuracy(knn_cm)

################################################################################
######################## CARAVAN DATA SET APPLICATION ##########################
################################################################################

attach(Caravan)
summary(Purchase)[2] / sum(summary(Purchase))
# Note that since only ~6% of the our observations would be considered a success
# (success = 1 = purchased caravan insurance), some sort of sampling procedure 
# should be considered (undersampling, oversampling, SMOAT) for deeper analysis
stand.X <- scale(Caravan[, -86])
test = 1:1000
train.X <- stand.X[-test, ]
test.X <- stand.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
# k = 1
knn_predictions <- knn(train.X, test.X, train.Y, k = 1)
knn_cm <- table(knn_predictions, test.Y)
knn_cm
accuracy(knn_cm)
recall(knn_cm)
precision(knn_cm)
# k = 3
knn_predictions <- knn(train.X, test.X, train.Y, k = 3)
knn_cm <- table(knn_predictions, test.Y)
knn_cm
accuracy(knn_cm)
recall(knn_cm)
precision(knn_cm)
# k = 5
knn_predictions <- knn(train.X, test.X, train.Y, k = 5)
knn_cm <- table(knn_predictions, test.Y)
knn_cm
accuracy(knn_cm)
recall(knn_cm)
precision(knn_cm)
# Logistic Regression
log_mod <- glm(Purchase ~ ., data=Caravan, family = "binomial", subset = -test)
log_mod_probs <- predict(log_mod, Caravan[test, ], type="response")
log_mod_predictions <- rep("No", 1000)
log_mod_predictions[log_mod_probs > 0.25] <- "Yes"
logistic_cm <- table(log_mod_predictions, test.Y)
accuracy(logistic_cm)
recall(logistic_cm)
precision(logistic_cm)