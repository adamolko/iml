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
library(mctest)

set.seed(123)
ada_check = FALSE

#Tell me if that works :)
if(ada_check){
  path = ""
  source(cleaning_path_train)
} else{
  path ="C:/R - Workspace/IML"
}

data = paste0(path, "/train.csv")
cleaning_path = paste0(path, "/cleaning.R")
source(cleaning_path)
# path_train = "data/train.csv"
# source("cleaning_train.R")
data = housing

#LINEAR MODEL
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

#one-hot encoding
not_dummies <- c("LotArea", "YearBuilt", "TotalBsmtSF",
                 "GrLivArea", "porch_area", "SalePrice",
                 "FullBath", "HalfBath", "BedroomAbvGr",
                 "KitchenAbvGr", "GarageCars", "OverallQual", 
                 "OverallCond", "CentralAir", "PavedDrive",
                 "pool", "Remod")

dummies <- select(data, -not_dummies)
dmy <- dummyVars(" ~ .", data = dummies)
trsf <- data.frame(predict(dmy, newdata = dummies))
for (name in not_dummies) {
  trsf[name] <- data[name]
}

#removing one level for each dummy encoded categorical feature
# AND: after analysis with imcdiag(model_lm) also need to drop another category for BsmtQual & BsmtFinType1, because 
# both have a NO dummy in there and therefore we end up with perfect multicollinearity
# in theory, the "no basement" should be captured by non-linear effects of the basement size (TotalBsmtSF), which linear regression cant do
# therefore, for perfect modeling, we would probably have to add a basement dummy, indicating if a basement if exists or not
# this is a good talking point, when comparing models :)
reference_levels <- c("MSZoning.RH", "LotShape.IR2.5",
                      "Neighborhood.Blueste", "BldgType.Twnhs",
                      "RoofStyle.Other", "ExterQual.Fa", 
                      "ExterCond.3", "Foundation.Other",
                      "BsmtQual.No", "BsmtQual.TA", "BsmtFinType1.Unf", "BsmtFinType1.No",
                      "HeatingQC.Po", "KitchenQual.Fa",
                      "Functional.Maj", "FireplaceQu.No",
                      "SaleType.Other", "SaleCondition.Other",
                      "SeasonSold.w", "porch_type.no", "Electrical.SBrkr")
trsf <- select(trsf, -reference_levels)

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
#rmse of 29438.02
rmsle_lm <- rmsle(test_y, predictions_lm)
rmsle_lm
#rmsle 0.1541895

saveRDS(model_lm, paste0(path, "/results/linear_regression.rds"))
saveRDS(train, paste0(path, "/results/train_linear_regression.rds"))
#LASSO
#uses the same dummy encoding as lm

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
#29044.89
rmsle_lasso <- rmsle(test_y, predictions_lasso)
rmsle_lasso
#0.1496133

#REGRESSION TREES
#with one-hot encoding rmse is actually worse
train <- data[train_ind, ]
test <- data[-train_ind, ]
regtree_model <- rpart(formula = SalePrice ~ ., data = train)
pred <- predict(regtree_model, newdata = test)
rmse(test$SalePrice, pred)
#44788.71
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
  control = list(minsplit = 14, maxdepth = 5, cp = 0.01)
)

pred <- predict(optimal_tree, newdata = test)
rmse_regtree <- rmse(test$SalePrice, pred)
rmse_regtree
#42437.68 well, doesn't change that much
rmsle_regtree <- rmsle(test$SalePrice, pred)
rmsle_regtree
#0.2312028

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