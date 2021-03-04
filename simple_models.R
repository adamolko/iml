library(Hmisc)
library(caret)
library(Metrics)
library(readr)
library(plyr)
library(glmnet)
library(forcats)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ipred)
library(mctest)
library(randomForest)
library(tidyverse)
library(mlr)


path_train  <- "data/train.csv"
cleaning_path_train <- "cleaning_train.R"
source(cleaning_path_train)
data = housing

#-------------------------
#LINEAR MODEL
set.seed(123)

# smp_size <- floor(0.75 * nrow(data))
# train_ind <- sample(seq_len(nrow(data)), size = smp_size)

# train_ind had the tendency to change even with the seed, so we saved them for reproducibility 
# saveRDS(train_ind, "results/train_ind.rds")

train_ind <- readRDS("results/train_ind.rds")

#one-hot encoding
not_dummies <- c("LotArea", "YearBuilt", "TotalBsmtSF",
                 "GrLivArea", "porch_area", "SalePrice",
                 "FullBath", "HalfBath", "BedroomAbvGr",
                 "KitchenAbvGr", "GarageCars", "OverallQual", 
                 "OverallCond", "CentralAir", "PavedDrive",
                 "pool", "Remod", "ExterQual", "ExterCond", "BsmtQual", "HeatingQC",  "KitchenQual", "BsmtFinType1", "FireplaceQu")
dummies <- select(data, -all_of(not_dummies))

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  trsf[name] <- data[name]
}

#removing one level for each dummy encoded categorical feature to avoid perfect multicollinearity
reference_levels <- c("MSZoning.RH", "LotShape.IR2.5",
                      "Neighborhood.Blueste", "BldgType.Twnhs",
                      "RoofStyle.Other", "Foundation.Other",
                      "Functional.Maj",
                      "SaleType.Other", "SaleCondition.Other",
                      "SeasonSold.w", "porch_type.no", "Electrical.SBrkr")
trsf <- select(trsf, -all_of(reference_levels))

#75/25 train/test split
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice



#linear model without interactions
model_lm <- lm(SalePrice~., data = train)
summary(model_lm)

predictions_lm <- predict(model_lm, test_X)

rmse_lm <- rmse(test_y, predictions_lm)
rmse_lm

rmsle_lm <- rmsle(test_y, predictions_lm)
rmsle_lm

saveRDS(model_lm, "results/linear_regression.rds")
saveRDS(train, "results/train_linear_regression.rds")


#--------------------------------------
#LASSO
set.seed(123)
#choosing best lambda value based on training data
lambdas <- 10^seq(3, -3, by = -.1)
lasso_reg <- cv.glmnet(data.matrix(train_X), train_y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

#actually fitting the model
lasso_model <- glmnet(data.matrix(train_X), train_y, alpha = 1, lambda = lambda_best, standardize = TRUE)
predictions_lasso <- predict(lasso_model, s = lambda_best, newx = data.matrix(test_X))
rmse_lasso <- rmse(test_y, predictions_lasso)
rmse_lasso

rmsle_lasso <- rmsle(test_y, predictions_lasso)
rmsle_lasso


#--------------------------------------
#Random Forest
set.seed(123)

# train with the default parameters
random_forest<- randomForest(x = train_X, y=train_y)

pred_randomForest =  predict(random_forest, test_X)

rmse_randomForest <- rmse(test_y, pred_randomForest)
rmse_randomForest

rmsle_randomForest <- rmsle(test_y, pred_randomForest)
rmsle_randomForest


# The following section is optional, down below you can simply load tuning parameters from RDS file

#Small hyperparameter tuning:
traintask <- makeRegrTask(data = train,target = "SalePrice")
testtask <- makeRegrTask(data = test, target = "SalePrice")

lrn <- makeLearner("regr.randomForest",predict.type = "response")
params <- makeParamSet( makeIntegerParam("maxnodes",lower = 1,upper = 300), 
                        makeIntegerParam("nodesize",lower = 1,upper = 10), 
                        makeIntegerParam("ntree",lower = 200,upper = 500))

rdesc <- makeResampleDesc(iters = 5, "CV")
ctrl <- makeTuneControlRandom(budget = 100)

mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, 
                     par.set = params, control = ctrl, show.info = T)
saveRDS(mytune, "results/tuning_result_randomForest.rds")

# loading the results of the tuning
mytune <- readRDS("results/tuning_result_randomForest.rds")

#Run best model with these parameters
set.seed(123)
parameters = mytune$x 
lrn_tune <- setHyperPars(lrn,par.vals = parameters)  

random_forest <- train(learner = lrn_tune,task = traintask)
pred_randomForest <- predict(random_forest,testtask)

rmse_randomForest <- rmse(pred_randomForest$data$truth, pred_randomForest$data$response)
rmse_randomForest

rmsle_randomForest <- rmsle(pred_randomForest$data$truth, pred_randomForest$data$response)
rmsle_randomForest
