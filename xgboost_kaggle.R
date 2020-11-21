library(xgboost)
library(caret)
library(Metrics)
library(dplyr)
library(readr)
library(plyr)
library(glmnet)
library(tidyverse)
library(forcats)
library(corrplot)
library(mlr)

#loading the data
path_test = "test.csv"
path_train = "train.csv"
source("cleaning_train.R")
train = housing
source("cleaning_test.R")
test = housing

#one-hot encoding for train data
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF",
              "GrLivArea", "porch_area", "SalePrice",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars", "OverallQual", "OverallCond")
dummies <- select(train, -all_of(numerics))

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in numerics) {
  trsf[name] <- train[name]
}
train <- trsf
train_X <- select(train, -SalePrice)

#one-hot encoding for test
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF",
              "GrLivArea", "porch_area", "OverallCond",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars", "OverallQual")
dummies <- select(test, -all_of(numerics))

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in numerics) {
  trsf[name] <- test[name]
}
test <- trsf
#enforces the same column order
test[names(train_X)]

dtrain <- xgb.DMatrix(data = data.matrix(train_X), label=train$SalePrice)
dtest <- xgb.DMatrix(data = data.matrix(test))

#tried just running with best parameters from 75/25 train/test split
#rmsle 0.23721
parameters
model_xgboost <- xgboost(dtrain, booster = "gbtree", max.depth = 4, min_child_weight=8.49,
                         nround = 429, random_state = 41, eta = 0.0643,
                         subsample = 0.947, early_stopping_rounds = 9, colsample_bytree = 0.986)
#need the following, otherwise get "feature names are different" error
colnames(dtest) <- NULL
predictions_xgboost <- predict(model_xgboost, dtest)

#tuning
traintask <- makeRegrTask(data = train,target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle")

params <- makeParamSet( 
  # makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
            makeIntegerParam("max_depth",lower = 2L,upper = 10L), 
            makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
            makeNumericParam("subsample",lower = 0.5,upper = 1), 
            makeNumericParam("eta",lower = 0.05,upper = 0.4), 
            makeIntegerParam("nrounds",lower = 50,upper = 500), 
            makeIntegerParam("early_stopping_rounds",lower = 0,upper = 10), 
            makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc <- makeResampleDesc("CV",iters=5L)

ctrl <- makeTuneControlCMAES()

#parallelization
library(parallel)
library(parallelMap) 

parallelStart(mode="socket", cpu=8, level="mlr.tuneParams")

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     par.set = params, control = ctrl, show.info = T)
saveRDS(mytune, paste0(path, "/tuning_result.rds"))

mytune$x
#choosing the best parameters
parameters <- mytune$x
#adding data to the parameters
parameters[["data"]] <- dtrain
# model_xgboost_tuned <- do.call(xgboost,parameters)
model_xgboost_tuned <- xgboost(data = dtrain, booster = "gbtree", max_depth = 3,
                               min_child_weight = 1.22769, subsample = 0.6421185,
                               nrounds=235, early_stopping_rounds = 6,
                               colsample_bytree = 0.7056465)

predictions_xgboost_tuned <- predict(model_xgboost_tuned, dtest)
predictions_xgboost_tuned

#creating a dataframe to save to .csv file
pred <- data.frame(predictions_xgboost)
colnames(pred) <- "SalePrice"
pred$Id <- 1461:2919
pred <- pred[c("Id", "SalePrice")]
write.csv(pred, "test_output.csv", row.names = F)
