install.packages("ISLR")
install.packages("MASS")
library(ISLR)
library(MASS)
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

