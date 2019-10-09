## -----------------------------------------------------------------------------
## 
## Different global settings used for the experiments
## 
## -----------------------------------------------------------------------------
## Libraries
## -----------------------------------------------------------------------------

library(e1071)
library(randomForest)
library(glmnet)
library(gbm)
library(ggplot2)
library(MASS)

## -----------------------------------------------------------------------------
## List of models used in the experiments
## -----------------------------------------------------------------------------
model_list <- list("lm"            = lm,
                   "svm"           = svm,
                   "randomforest"  = randomForest)

## -----------------------------------------------------------------------------
## List of datasets used in the experiments
## -----------------------------------------------------------------------------

dataset_list <- list("airquality"           = "uci_airquality.rds",
                     "synthetic_03"         = "synthetic_03.rds", 
                     "synthetic_04"         = "synthetic_04.rds", 
                     "bike"  = "bike_trend_removed.rds",
                     "airline"              = "airline.rds",
                     "bike_current"              = "bike_current.rds")

## -----------------------------------------------------------------------------
## Matrix of dataset/k combinations
## -----------------------------------------------------------------------------

uci <- c(2, 10, 20, 80, 100)
synthetic <-c(2, 5, 10, 20, 60)
bike <- c(2, 3, 4, 5, 6)
airline <-c(2, 10, 20, 80, 100)
k_list <- cbind(uci, synthetic, synthetic, bike, airline, bike)
colnames(k_list) <- seq.int(dim(k_list)[2])

## -----------------------------------------------------------------------------
