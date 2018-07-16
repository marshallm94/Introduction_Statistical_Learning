library(MASS)
library(ISLR)

attach(Boston)
names(Boston)

# Simple Linear Regression
lm.fit <- lm(medv ~ lstat)
lm.fit
summary(lm.fit)
names(lm.fit)

confint(lm.fit)
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = 'confidence')
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = 'prediction')
# Note the difference between confidence and prediction interval width
plot(lstat, medv, pch = 16, col = "blue")
abline(lm.fit, col = "red", lwd = 3)

par(mfrow = c(2,2))
plot(lm.fit)

par(mfrow = c(1,1))
plot(predict(lm.fit), residuals(lm.fit), pch = 16, col = "blue")

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
library(car)
vif(lm.fit)

lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

# Interaction Terms
summary(lm(medv ~ lstat*age, data = Boston))

# Non-linear Transformations of the Predictors
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
lm.fit <- lm(medv ~ lstat, data = Boston)
summary(lm.fit2)
anova(lm.fit, lm.fit2)
par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)
summary(lm(medv ~ log(lstat), data = Boston))

# Qualitative Predictors
attach(Carseats)
names(Carseats)
lm.fit <- lm(Sales ~ . +Income:Advertising + Price: Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc)

# Writing Functions
LoadLibraries <- function() {
    library(ISLR)
    library(MASS)
}
LoadLibraries()