library(xgboost)
library(caret)
library(Metrics)
library(dplyr)
library(readr)
library(plyr)
library(glmnet)
#loading and cleaning data a bit
data <- read.csv("clean_data.csv")
data <- select(data, -X)
str(data)

#LINEAR MODEL
#train test split
smp_size <- floor(0.75 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

model_lm <- lm(SalePrice~., data = train)
summary(model_lm)
predictions_lm <- predict(model_lm, test_X)

rmse_lm <- rmse(test_y, predictions_lm)
#rmse of 27400.36
rmsle_lm <- rmsle(test_y, predictions_lm)
#rmsle 0.1427626

#XGBOOST
#need some data transformations for xgboost
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF", 
                 "GrLivArea", "porch_area", "SalePrice")
dummies <- select(data, -numerics)

dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  trsf[name] <- data[name]
}

train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

dtrain <- xgb.DMatrix(data = data.matrix(train_X), label=train_y)
dtest <- xgb.DMatrix(data = data.matrix(test_X), label=test_y)
model_xgboost <- xgboost(dtrain, max.depth = 5, nround = 1000)
#train ramse is 301
predictions_xgboost <- predict(model_xgboost, dtest)
rmse_xgboost <- rmse(test_y, predictions_xgboost)
rmse_xgboost
#rmse 25145.49 - without encoding OverallQual and stuff like that
#rmse 25054.61 - with encoding
rmsle_xgboost <- rmsle(test_y, predictions_xgboost)
#0.1312619

#LASSO
#need values scaling
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF", 
              "GrLivArea", "porch_area", "SalePrice")
pre_proc_val <- preProcess(data[,numerics], method = c("center", "scale"))
data[,numerics] <- predict(pre_proc_val, data[,numerics])

#doing one-hot encoding again just in case
dummies <- select(data, -numerics)
dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  trsf[name] <- data[name]
}
train <- trsf[train_ind, ]
test <- trsf[-train_ind, ]
train_X <- select(train, -SalePrice)
train_y <- train$SalePrice
test_X <- select(test, -SalePrice)
test_y <- test$SalePrice

#choosing best lambda value based on training data
lambdas <- 10^seq(2, -3, by = -.1)
lasso_reg <- cv.glmnet(data.matrix(train_X), train_y, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)
lambda_best <- lasso_reg$lambda.min 
lambda_best

#actually fitting the model
lasso_model <- glmnet(data.matrix(train_X), train_y, alpha = 1, lambda = lambda_best, standardize = TRUE)
predictions_lasso <- predict(lasso_model, s = lambda_best, newx = data.matrix(test_X))
rmse_lasso <- rmse(test_y, predictions_lasso)
#0.33???????????????????????????????????????????????????????
rmsle_lasso <- rmsle(test_y, predictions_lasso)
#says there are nans, idk
