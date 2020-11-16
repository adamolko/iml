library(caret)
library(Metrics)
library(dplyr)
library(readr)
library(plyr)
library(glmnet)
library(tidyverse)
library(forcats)
library(corrplot)
library(rpart)
library(rpart.plot)
library(ipred)

# path ="C:/R - Workspace/IML"
# data = paste0(path, "/train.csv")
# cleaning_path = paste0(path, "/cleaning.R")
# source(cleaning_path)
source("cleaning.R")
data = housing

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
rmse_lm
#rmse of 25763.99
rmsle_lm <- rmsle(test_y, predictions_lm)
rmsle_lm
#rmsle 0.1387397

#LASSO
numerics <- c("LotArea", "YearBuilt", "TotalBsmtSF", 
              "GrLivArea", "porch_area", "SalePrice",
              "FullBath", "HalfBath", "BedroomAbvGr",
              "KitchenAbvGr", "GarageCars")

#one-hot encoding 
dummies <- select(data, -numerics)
dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in numerics) {
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
rmse_lasso
#26248.91
rmsle_lasso <- rmsle(test_y, predictions_lasso)
rmsle_lasso

#REGRESSION TREES
#with one-hot encoding rmse is actually worse
train <- data[train_ind, ]
test <- data[-train_ind, ]
regtree_model <- rpart(formula = SalePrice ~ ., data = train)
pred <- predict(regtree_model, newdata = test)
rmse(test$SalePrice, pred)
#38993.68
rpart.plot(regtree_model)
#cp - cost complexity value
plotcp(regtree_model)

#tuning
hyper_grid <- expand.grid(
  minsplit = seq(1, 20, 1),
  maxdepth = seq(1, 15, 1),
  method = c("anova", "poisson", "exp", "class")
)
models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  method <- hyper_grid$method[i]
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = SalePrice ~ .,
    data    = train,
    control = list(minsplit = minsplit, maxdepth = maxdepth, method = method)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

#if you choose the first model with anova, the error doesn't go down
#method = "exp" requires the target variable to be of type survival, which is not our case
#=> minsplit = 1, maxdepth = 13, cp = 0.01, method = poisson

optimal_tree <- rpart(
  formula = SalePrice ~ .,
  data    = train,
  method = "poisson",
  control = list(minsplit = 1, maxdepth = 13, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = test)
rmse_regtree <- rmse(test$SalePrice, pred)
rmse_regtree
#38659.02 well, doesn't change that much
rmsle_regtree <- rmsle(test$SalePrice, pred)
rmsle_regtree
#0.2136424

#BAGGED TREES
ctrl <- trainControl(method = "cv",  number = 10) 

# CV bagged model
bagged_cv <- train(
  SalePrice ~ .,
  data = train,
  method = "treebag",
  trControl = ctrl,
  importance = TRUE
)

# assess results
bagged_cv
# plot most important variables
plot(varImp(bagged_cv), 20)  

predictions_bagging <- predict(bagged_cv, test)
rmse_bagging <- rmse(test$SalePrice, predictions_bagging)
rmse_bagging
#36043.38

rmsle_bagging <- rmsle(test$SalePrice, predictions_bagging)
rmsle_bagging
#0.20843