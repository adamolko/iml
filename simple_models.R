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



ada_check = FALSE
if(ada_check){
  path = ""
} else{
  path ="D:/R - Workspace/IML"
}
path_train = paste0(path, "/data/train.csv")
cleaning_path_train = paste0(path, "/cleaning_train.R")
source(cleaning_path_train)
data = housing

#-------------------------
#LINEAR MODEL
set.seed(123)
smp_size <- floor(0.75 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

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
#rmse of 34000.24
rmsle_lm <- rmsle(test_y, predictions_lm)
rmsle_lm
#rmsle 0.1727657

saveRDS(model_lm, paste0(path, "/results/linear_regression.rds"))
saveRDS(train, paste0(path, "/results/train_linear_regression.rds"))
#--------------------------------------
#LASSO
#uses the same dummy encoding as lm
set.seed(123)
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
#33906.95
rmsle_lasso <- rmsle(test_y, predictions_lasso)
rmsle_lasso
#0.1705668


#--------------------------------------
#Random Forest
set.seed(123)
random_forest<- randomForest(x = train_X, y=train_y)
random_forest

pred_randomForest =  predict(random_forest, test_X)

rmse_randomForest <- rmse(test_y, pred_randomForest)
rmse_randomForest
#28236.06
rmsle_randomForest <- rmsle(test_y, pred_randomForest)
rmsle_randomForest
#0.135233












#-----------------------------------
#OTHER MODELS


#--------------------------------------
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

#--------------------------------------
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