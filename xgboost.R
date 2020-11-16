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
source("cleaning.R")
data = read_csv("clean_data.csv")
data = select(data, -X1)
#need some data transformations for xgboost
# numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF", 
              # "GrLivArea", "porch_area", "SalePrice")

numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF", 
              "GrLivArea", "porch_area", "SalePrice",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars")
dummies <- select(data, -numerics)

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in numerics) {
  trsf[name] <- data[name]
}
#sampling train/test 75/25
smp_size <- floor(0.75 * nrow(trsf))
set.seed(123)
train_ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

dtrain <- xgb.DMatrix(data = data.matrix(train_X), label=train_y)
dtest <- xgb.DMatrix(data = data.matrix(test_X), label=test_y)
model_xgboost <- xgboost(dtrain, max.depth = 5, nround = 1000)
#train rmse is 300
predictions_xgboost <- predict(model_xgboost, dtest)
rmse_xgboost <- rmse(test_y, predictions_xgboost)
rmse_xgboost
#rmse 27223
rmsle_xgboost <- rmsle(test_y, predictions_xgboost)
rmsle_xgboost
#0.147


#tuning
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test,target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle", nrounds=100L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","gblinear")), 
                        makeIntegerParam("max_depth",lower = 3L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc <- makeResampleDesc("CV",iters=5L)
ctrl <- makeTuneControlRandom(maxit = 10L)

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     par.set = params, control = ctrl, show.info = T)
mytune$y
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)
xgmodel <- train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)
print(xgpred)
rmse_tuned_xgboost <- rmse(test$SalePrice, xgpred$data$response)
rmse_tuned_xgboost
#24239.75
rmsle_tuned_xgboost <- rmsle(test$SalePrice, xgpred$data$response)
rmsle_tuned_xgboost
#0.131