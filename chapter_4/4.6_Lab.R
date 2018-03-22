install.packages("ISLR")
library(ISLR)
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
accuracy <- sum(cm[1,1], cm[2,2]) / sum(cm)
precision <- cm[2,2] / sum(cm[2,])
recall <- cm[2,2] / sum(cm[,2])

# Train-Test Split
train <- (Year < 2005)
Smarket.2005 <-  Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <-  Direction[!train]