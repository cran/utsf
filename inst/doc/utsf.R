## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(utsf)

## -----------------------------------------------------------------------------
m <- create_model(AirPassengers, lags = 1:12, method = "rt")
f <- forecast(m, h = 12)

## -----------------------------------------------------------------------------
f$pred
library(ggplot2)
autoplot(f)

## -----------------------------------------------------------------------------
head(m$targets)  # first targets
head(m$features) # and its associated features

## -----------------------------------------------------------------------------
t <- ts(c(1, 3, 6, 7, 9, 11, 16))
out <- create_model(t, lags = c(1, 2, 4), trend = "none")
cbind(out$features, Target = out$targets)

## -----------------------------------------------------------------------------
m <- create_model(USAccDeaths, method = "mt")
f <- forecast(m, h = 12, PI = TRUE, level = 90)
f
library(ggplot2)
autoplot(f)

## -----------------------------------------------------------------------------
m <- create_model(fdeaths, lags = 1:12, method = "rt")
m$model

## -----------------------------------------------------------------------------
# Function to train the regression model
# In this case (k-NN) just stores the training set
my_knn_model <- function(X, y, param) {
  structure(list(X = X, y = y), class = "my_knn")
}

# Function to predict a new example
predict.my_knn <- function(object, new_value) {
  FNN::knn.reg(train = object$X, test = new_value, y = object$y)$pred
}

m <- create_model(AirPassengers, lags = 1:12, method = my_knn_model)
f <- forecast(m, h = 12)
f$pred
autoplot(f)

## -----------------------------------------------------------------------------
# Function to train the regression model
my_knn_model2 <- function(X, y, param) {
  structure(list(X = X, y = y), class = "my_knn2")
}

# Function to predict a new example
predict.my_knn2 <- function(object, new_value) {
  k <- 3 # number of nearest neighbors
  distances <- sapply(1:nrow(object$X), function(i) sum((object$X[i, ] - new_value)^2))
  k_nearest <- order(distances)[1:k]
  mean(object$y[k_nearest])
}

m2 <- create_model(AirPassengers, lags = 1:12, method = my_knn_model2)
forecast(m2, h = 12)$pred

## -----------------------------------------------------------------------------
library(randomForest)
my_model <- function(X, y, param) { randomForest(x = X, y = y) }
m <- create_model(USAccDeaths, method = my_model)
f <- forecast(m, h = 12)
library(ggplot2)
autoplot(f)
print(m$model)

## -----------------------------------------------------------------------------
library(nnet)
my_model <- function(X, y, param) {
  nnet(x = X, y = y, size = 5, linout = TRUE, trace = FALSE)
}
m <- create_model(USAccDeaths, method = my_model)
f <- forecast(m, h = 12)
library(ggplot2)
autoplot(f)

## -----------------------------------------------------------------------------
library(xgboost)
my_model <- function(X, y, param) {
  m <- xgboost(data = as.matrix(X), 
               label = y, 
               nrounds = 100,
               verbose = 0
  )
  structure(m, class = "my_model")
}
predict.my_model <- function(object, new_value) {
  class(object) <- "xgb.Booster"
  predict(object, as.matrix(new_value))
}
m <- create_model(USAccDeaths, method = my_model)
f <- forecast(m, h = 12)
library(ggplot2)
autoplot(f)

## -----------------------------------------------------------------------------
# A bagging model set with default parameters
m <- create_model(AirPassengers, lags = 1:12, method = "bagging")
length(m$model$mtrees) # number of regression trees (25 by default)
# A bagging model set with 3 regression trees
m2 <- create_model(AirPassengers,
                   lags = 1:12,
                   method = "bagging",
                   param = list(nbagg = 3)
)
length(m2$model$mtrees) # number of regression trees

## -----------------------------------------------------------------------------
# Function to train the model
my_knn_model <- function(X, y, param) {
  k <- if ("k" %in% names(param)) param$k else 3
  structure(list(X = X, y = y, k = k), class = "my_knn")
}

# Regression function for object of class my_knn
predict.my_knn <- function(object, new_value) {
  FNN::knn.reg(train = object$X, test = new_value,
               y = object$y, k = object$k)$pred
}

# The model is trained with default parameters (k = 3)
m <- create_model(AirPassengers, lags = 1:12,  method = my_knn_model)
print(m$model$k)
# The model is trained with k = 5
m2 <- create_model(AirPassengers, method = my_knn_model, param = list(k = 5))
print(m2$model$k)

## -----------------------------------------------------------------------------
library(randomForest)
my_model <- function(X, y, param) {
    args <- list(x = X, y = y, ntree = 200) # default parameters
    args <- args[!(names(args) %in% names(param))]
    args <- c(args, param)
    do.call(randomForest::randomForest, args = args)
}
# The random forest is built with our default parameters
m <- create_model(USAccDeaths, method = my_model)
print(m$model$ntree)
print(m$model$mtry)
# The random forest is built with ntree = 400 and mtry = 6
m <- create_model(USAccDeaths, method = my_model, 
                  param = list(ntree = 400, mtry = 6))
print(m$model$ntree)
print(m$model$mtry)

## -----------------------------------------------------------------------------
m <- create_model(UKgas, lags = 1:4, method = "knn")
r <- efa(m, h = 4, type = "normal", size = 8)
r$per_horizon
r$global

## ----out.width = '85%', echo = FALSE------------------------------------------
knitr::include_graphics("ro_normal.png")

## ----eval = FALSE-------------------------------------------------------------
# m <- create_model(UKgas, lags = 1:4, method = "knn")
# # Use the last 9 observations of the series to build test sets
# r1 <- efa(m, h = 4, type = "normal", size = 9)
# # Use the last 20% observations of the series to build test sets
# r2 <- efa(m, h = 4, type = "normal", prop = 0.2)

## -----------------------------------------------------------------------------
m <- create_model(UKgas, lags = 1:4, method = "knn")
r <- efa(m, h = 4, type = "minimum")
r$per_horizon
r$global

## ----out.width = '85%', echo = FALSE------------------------------------------
knitr::include_graphics("ro_minimum.png")

## -----------------------------------------------------------------------------
timeS <- ts(1:25)
m <- create_model(timeS, lags = 1:3, method = "mt")
r <- efa(m, h = 5, size = 7)
r$test_sets
r$predictions

## -----------------------------------------------------------------------------
timeS <- ts(1:25)
m <- create_model(timeS, lags = 1:3, method = "mt")
r <- efa(m, h = 3, type = "minimum")
r$test_sets
r$predictions

## -----------------------------------------------------------------------------
m <- create_model(UKgas, lags = 1:4, method = "knn")
r <- tune_grid(m, h = 4, tuneGrid = expand.grid(k = 1:7), type = "normal", size = 8)
# To see the estimated forecast accuracy with the different configurations
r$tuneGrid

## -----------------------------------------------------------------------------
r$best
r$forecast

## -----------------------------------------------------------------------------
plot(r$tuneGrid$k, r$tuneGrid$RMSE, type = "o", pch = 19, xlab = "k (number of nearest neighbors)", ylab = "RMSE", main = "Estimated accuracy")

## -----------------------------------------------------------------------------
m <- create_model(UKDriverDeaths, lags = 1:12, method = "rf")
r <- tune_grid(m, 
               h = 12, 
               tuneGrid = expand.grid(num.trees = c(200, 500), replace = c(TRUE, FALSE), mtry = c(4, 8)),
               type = "normal", 
               size = 12
)
r$tuneGrid

## -----------------------------------------------------------------------------
m <- create_model(airmiles, lags = 1:4, method = "rf", trend = "none")
f <- forecast(m, h = 4)
autoplot(f)

## -----------------------------------------------------------------------------
m <- create_model(airmiles, lags = 1:4, method = "rf", trend = "differences", nfd = 1)
f <- forecast(m, h = 4)
autoplot(f)

## -----------------------------------------------------------------------------
# The order of first differences is estimated using the ndiffs function
m <- create_model(airmiles, lags = 1:4, method = "rf", trend = "differences", nfd = -1)
m$differences

## -----------------------------------------------------------------------------
timeS <- ts(c(1, 3, 7, 9, 10, 12))
m <- create_model(timeS, lags = 1:2, trend = "none")
cbind(m$features, Targets = m$targets)

## -----------------------------------------------------------------------------
timeS <- ts(c(1, 3, 7, 9, 10, 12))
m <- create_model(timeS, lags = 1:2, trend = "additive", transform_features = FALSE)
cbind(m$features, Targets = m$targets)

## -----------------------------------------------------------------------------
timeS <- ts(c(1, 3, 7, 9, 10, 12))
m <- create_model(timeS, lags = 1:2, trend = "additive", transform_features = TRUE)
cbind(m$features, Targets = m$targets)

## -----------------------------------------------------------------------------
m <- create_model(airmiles, lags = 1:4, method = "rf")
f <- forecast(m, h = 4)
autoplot(f)

## -----------------------------------------------------------------------------
t <- ts(10 * 1.05^(1:20))
m_m <- create_model(t, lags = 1:3, method = "rf", trend = "multiplicative")
f_m <- forecast(m_m, h = 4)
m_a <- create_model(t, lags = 1:3, method = "rf", trend = "additive")
f_a <- forecast(m_a, h = 4)
library(vctsfr)
plot_predictions(t, predictions = list(Multiplicative = f_m$pred, Additive = f_a$pred))

## ----fig.width= 6-------------------------------------------------------------
m_m <- create_model(AirPassengers, lags = 1:12, method = "knn", trend = "multiplicative")
f_m <- forecast(m_m, h = 36)
m_a <- create_model(AirPassengers, lags = 1:12, method = "knn", trend = "additive")
f_a <- forecast(m_a, h = 36)
library(vctsfr)
plot_predictions(AirPassengers, predictions = list(Multiplicative = f_m$pred, Additive = f_a$pred))

