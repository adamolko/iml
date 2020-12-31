library(Hmisc)
library(xgboost)
library(caret)
library(Metrics)
library(plyr)
library(dplyr)
library(readr)
library(glmnet)
library(tidyverse)
library(forcats)
library(corrplot)
library(mlr)
library(cmaes)

set.seed(123)

#ada_check = TRUE
ada_check = FALSE

if(ada_check){
  path = ""
} else{
  path ="C:/R - Workspace/IML"
  
}
path_train = paste0(path, "/data/train.csv")
cleaning_path_train = paste0(path, "/cleaning_train.R")
source(cleaning_path_train)

train = housing

#one-hot encoding for train
not_dummies <- c("LotArea", "YearBuilt", "TotalBsmtSF",
              "GrLivArea", "porch_area", "SalePrice",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars", "OverallQual", 
              "OverallCond", "CentralAir", "PavedDrive",
              "pool", "Remod", "ExterQual", "ExterCond", "BsmtQual", "HeatingQC", 
              "KitchenQual", "BsmtFinType1", "FireplaceQu", "numb_add_flr")
dummies <- select(train, -all_of(not_dummies))
dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  print(name)
  trsf[name] <- train[name]
}
#for interpretation purposes, should drop one dummy for each categorical variable:
reference_levels <- c("MSZoning.RH", "LotShape.IR2.5",
                      "Neighborhood.Blueste", "BldgType.Twnhs",
                      "RoofStyle.Other",
                      "Foundation.Other",
                      "Functional.Maj",
                      "SaleType.Other", "SaleCondition.Other",
                      "SeasonSold.w", "porch_type.no", "Electrical.SBrkr")
trsf <- select(trsf, -all_of(reference_levels))

train <- trsf
#sampling train/test 75/25
smp_size <- floor(0.75 * nrow(trsf))
train_ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]

#--------------------------------
#Simple example without tuning:
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

dtrain <- xgb.DMatrix(data = data.matrix(train_X), label=train_y)
dtest <- xgb.DMatrix(data = data.matrix(test_X),  label=test_y)

model_xgboost <- xgboost(dtrain, max.depth = 3, nround = 300)
#need this because otherwise "feature names do not coincide" error
colnames(dtest) <- NULL
predictions_xgboost <- predict(model_xgboost, dtest)
rmse_xgboost <- rmse(test_y, predictions_xgboost)
rmse_xgboost

rmsle_xgboost <- rmsle(test_y, predictions_xgboost)
rmsle_xgboost



#------------------------------
#Now do tuning:
#Takes a long time!!!
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list(eval_metric="rmsle") # objective="reg:squarederror", 

params <- makeParamSet( makeIntegerParam("max_depth",lower = 2L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 1L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("eta",lower = 0.01,upper = 0.4), 
                        makeIntegerParam("nrounds",lower = 50,upper = 500), 
                        makeIntegerParam("early_stopping_rounds",lower = 0,upper = 10), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
rdesc <- makeResampleDesc("CV",iters=10L)
ctrl <- makeTuneControlCMAES()

#parallelization
library(parallel)
library(parallelMap) 

parallelStart(mode="socket", cpu=8, level="mlr.tuneParams")
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     par.set = params, control = ctrl, show.info = T)
saveRDS(mytune, paste0(path, "/tuning_result.rds"))
mytune$x

#--------------------------
#Now run model with best parameters:
set.seed(123)
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle")
tuning_result <- readRDS(paste0(path, "/results/tuning_result.rds"))
parameters = tuning_result$x 
lrn_tune <- setHyperPars(lrn,par.vals = parameters)  

xgmodel <- train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)

rmse_xgboost <- rmse(xgpred$data$truth, xgpred$data$response)
rmse_xgboost
rmsle_xgboost <- rmsle(xgpred$data$truth, xgpred$data$response)
rmsle_xgboost

#save training data & model for IML stuff
saveRDS(xgmodel, paste0(path, "/results/xgboost_model.rds"))
saveRDS(train, paste0(path, "/results/training_data.rds"))



#########################
#Now run xgboost with best parameters and all variables:
set.seed(123)
train = housing_full
train = train %>% rename("first_floor_sf" = "1stFlrSF", "second_floor_sf" = "2ndFlrSF")
#one-hot encoding for train
not_dummies <- c("LotArea", "YearBuilt", "TotalBsmtSF",
                 "GrLivArea", "porch_area", "SalePrice",
                 "FullBath", "HalfBath", "BedroomAbvGr",
                 "KitchenAbvGr", "GarageCars", "OverallQual", 
                 "OverallCond", "CentralAir", "PavedDrive",
                 "Remod", "ExterQual", "ExterCond", "BsmtQual", "HeatingQC", 
                 "KitchenQual", "BsmtFinType1", "FireplaceQu",
                 
                 "Fireplaces", "GarageArea", "GarageYrBlt", 
                 "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "MoSold",
                 "LotFrontage", "MasVnrArea", "first_floor_sf", "second_floor_sf",
                 "TotRmsAbvGrd", "PoolArea", "LowQualFinSF", 
                 "YrSold", "MiscVal", "GarageQual"    # "BsmtCond", "BsmtFullBath", "BsmtHalfBath",
                 )
dummies <- select(train, -all_of(not_dummies))

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  trsf[name] <- train[name]
}
train <- trsf

#sampling train/test 75/25
smp_size <- floor(0.75 * nrow(trsf))
train_ind <- sample(seq_len(nrow(trsf)), size = smp_size)
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]

traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")

lrn <- makeLearner("regr.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="reg:squarederror", eval_metric="rmsle")
tuning_result <- readRDS(paste0(path, "/results/tuning_result.rds"))
parameters = tuning_result$x 
lrn_tune <- setHyperPars(lrn,par.vals = parameters)  

xgmodel <- train(learner = lrn_tune,task = traintask)
xgpred <- predict(xgmodel,testtask)

rmse_xgboost <- rmse(xgpred$data$truth, xgpred$data$response)
rmse_xgboost
rmsle_xgboost <- rmsle(xgpred$data$truth, xgpred$data$response)
rmsle_xgboost




