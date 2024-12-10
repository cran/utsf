## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(utsf)

## -----------------------------------------------------------------------------
f <- forecast(AirPassengers, h = 12, lags = 1:12, method = "rt")

## -----------------------------------------------------------------------------
f$pred
library(ggplot2)
autoplot(f)

## -----------------------------------------------------------------------------
head(f$targets)  # first targets
head(f$features) # and its associated features

## -----------------------------------------------------------------------------
t <- ts(c(1, 3, 6, 7, 9, 11, 16))
out <- forecast(t, h = 3, lags = c(1, 2, 4), preProcess = list(trend("none")))
cbind(out$features, Target = out$targets)

## -----------------------------------------------------------------------------
f <- forecast(fdeaths, h = 12, lags = 1:12, method = "rt")
f$model

## -----------------------------------------------------------------------------
# Function to train the regression model
my_knn_model <- function(X, y, k = 3) {
  structure(list(X = X, y = y, k = k), class = "my_knn")
}

# Function to predict a new example
predict.my_knn <- function(object, new_value) {
  FNN::knn.reg(train = object$X, test = new_value, 
               y = object$y, k = object$k)$pred
}

f <- forecast(AirPassengers, h = 12, lags = 1:12, method = my_knn_model)
f$pred
autoplot(f)

## -----------------------------------------------------------------------------
# Function to train the regression model
my_knn_model2 <- function(X, y, k = 3) {
  structure(list(X = X, y = y, k = k), class = "my_knn2")
}

# Function to predict a new example
predict.my_knn2 <- function(object, new_value) {
  distances <- sapply(1:nrow(object$X), function(i) sum((object$X[i, ] - new_value)^2))
  k_nearest <- order(distances)[1:object$k]
  mean(object$y[k_nearest])
}

f2 <- forecast(AirPassengers, h = 12, lags = 1:12, method = my_knn_model2)
f2$pred

## -----------------------------------------------------------------------------
# A bagging model set with default parameters
f <- forecast(AirPassengers, h = 12, lags = 1:12, method = "bagging")
length(f$model$mtrees) # number of regression trees (25 by default)
# A bagging model set with 3 regression tress
f <- forecast(AirPassengers, h = 12, 
              lags = 1:12, 
              method = "bagging", 
              param = list(nbagg = 3)
)
length(f$model$mtrees) # number of regression trees

## -----------------------------------------------------------------------------
# Function to train the model
my_knn_model <- function(X, y, k = 3) {
  structure(list(X = X, y = y, k = k), class = "my_knn")
}

# Regression function for object of class my_knn
predict.my_knn <- function(object, new_value) {
  FNN::knn.reg(train = object$X, test = new_value, 
               y = object$y, k = object$k)$pred
}

# The model is trained with default parameters (k = 3)
f <- forecast(AirPassengers, h = 12, lags = 1:12,  method = my_knn_model)
print(f$model$k)
# The model is trained with k = 5
f <- forecast(AirPassengers, h = 12, 
              method = my_knn_model, param = list(k = 5))
print(f$model$k)

## -----------------------------------------------------------------------------
f <- forecast(UKgas, h = 4, lags = 1:4, method = "knn", efa = evaluation("normal", size = 8))
f$efa 

## -----------------------------------------------------------------------------
evaluation("normal", size = 10) # The last 10 observations are used as test set
evaluation("normal", prop = 0.2) # The last 20% part of the series is used as test set

## -----------------------------------------------------------------------------
f <- forecast(UKgas, h = 4, lags = 1:4, method = "knn", 
              tuneGrid = expand.grid(k = 1:7), efa = evaluation("normal", size = 4))
f$tuneGrid 

## -----------------------------------------------------------------------------
f$param
f$pred 

## -----------------------------------------------------------------------------
plot(f$tuneGrid$k, f$tuneGrid$RMSE, type = "o", pch = 19, xlab = "k (number of nearest neighbors)", ylab = "RMSE", main = "Estimated accuracy")

## -----------------------------------------------------------------------------
f <- forecast(airmiles, h = 4, lags = 1:4, method = "rf", preProcess = list(trend("none")))
autoplot(f)

## -----------------------------------------------------------------------------
f <- forecast(airmiles, h = 4, lags = 1:4, method = "rf", preProcess = list(trend("differences", 1)))
autoplot(f)

## -----------------------------------------------------------------------------
# The order of first differences is estimated using the ndiffs function
f <- forecast(airmiles, h = 4, lags = 1:4, method = "rf", preProcess = list(trend("differences", -1)))
f$differences

## -----------------------------------------------------------------------------
timeS <- ts(c(1, 3, 7, 9, 10, 12))
f <- forecast(timeS, h = 1, lags = 1:2, preProcess = list(trend("none")))
cbind(f$features, Targets = f$targets)

## -----------------------------------------------------------------------------
timeS <- ts(c(1, 3, 7, 9, 10, 12))
f <- forecast(timeS, h = 1, lags = 1:2, preProcess = list(trend("additive")))
cbind(f$features, Targets = f$targets)

## -----------------------------------------------------------------------------
f <- forecast(airmiles, h = 4, lags = 1:4, method = "rf")
autoplot(f)

## -----------------------------------------------------------------------------
t <- ts(10 * 1.05^(1:20))
f_m <- forecast(t, h = 4, lags = 1:3, method = "rf", preProcess = list(trend("multiplicative")))
f_a <- forecast(t, h = 4, lags = 1:3, method = "rf", preProcess = list(trend("additive")))
library(vctsfr)
plot_predictions(t, predictions = list(Multiplicative = f_m$pred, Additive = f_a$pred))

