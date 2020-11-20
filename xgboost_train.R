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
library(cmaes)


# path ="C:/R - Workspace/IML"
# path_test = paste0(path, "/test.csv")
# path_train = paste0(path, "/train.csv")
# cleaning_path_test = paste0(path, "/cleaning_test.R")
# cleaning_path_train = paste0(path, "/cleaning_train.R")

path_train = "train.csv"
set.seed(123)
# source(cleaning_path_train)
source("cleaning_train.R")
train = housing


#one-hot encoding for train
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF",
              "GrLivArea", "porch_area", "SalePrice",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars")
# numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF",
#               "GrLivArea", "porch_area", "SalePrice",
#               "FullBath", "HalfBath", "BedroomAbvGr",
#               "KitchenAbvGr", "GarageCars", "QualCond")
dummies <- select(train, -all_of(numerics))

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in numerics) {
  trsf[name] <- train[name]
}
train <- trsf

#-----
#Simple example without tuning:
#sampling train/test 75/25
smp_size <- floor(0.75 * nrow(trsf))
train_ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]

train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

dtrain <- xgb.DMatrix(data = data.matrix(train_X), label=train_y)
dtest <- xgb.DMatrix(data = data.matrix(test_X),  label=test_y)

model_xgboost <- xgboost(dtrain, max.depth = 3, nround = 500)
#need this because otherwise "feature names do not coincide" error
colnames(dtest) <- NULL
predictions_xgboost <- predict(model_xgboost, dtest)
rmse_xgboost <- rmse(test_y, predictions_xgboost)
rmse_xgboost
#rmse 24867.64
rmsle_xgboost <- rmsle(test_y, predictions_xgboost)
rmsle_xgboost
#0.1331943

#-------
#Now with tuning:
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle", eta=0.1)
#lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle", nrounds=100L, eta=0.1)

params <- makeParamSet( 
  # makeDiscreteParam("booster",values = c("gbtree","gblinear")),
                        makeIntegerParam("max_depth",lower = 2L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                       # makeNumericParam("eta",lower = 0.1,upper = 0.4), 
                        makeIntegerParam("nrounds",lower = 50,upper = 300), 
                        makeIntegerParam("early_stopping_rounds",lower = 0,upper = 10), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc <- makeResampleDesc("CV",iters=5L)

#makeTuneControlCMAES only works with int and num parameters,
#had to remove learner type
ctrl <- makeTuneControlCMAES()

#parallelization
library(parallel)
library(parallelMap) 
# parallelStartSocket(cpus = detectCores())
parallelStart(mode="multicore", cpu=4, level="mlr.tuneParams")

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     par.set = params, control = ctrl, show.info = T)
mytune$x
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)
xgmodel <- train(learner = lrn_tune,task = traintask)



xgpred <- predict(xgmodel,testtask)

rmse_xgboost <- rmse(xgpred$data$truth, xgpred$data$response)
rmse_xgboost
rmsle_xgboost <- rmsle(xgpred$data$truth, xgpred$data$response)
rmsle_xgboost
#0.122832
#best so far:
# "gbtree"
# max_depth: 3
# min_child_weight: 1.22769
# subsample: 0.6421185
# nrounds: 235
# early_stopping_rounds :6
# colsample_bytree: 0.7056465

#best = mytune$x

#try it with best parameters:
set.seed(2345)
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")
lrn_tune <- setHyperPars(lrn,par.vals = best)
xgmodel <- train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)

rmse_xgboost <- rmse(xgpred$data$truth, xgpred$data$response)
rmse_xgboost
rmsle_xgboost <- rmsle(xgpred$data$truth, xgpred$data$response)
rmsle_xgboost
#0.1214202