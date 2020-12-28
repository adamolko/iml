library(xgboost)
library(tidyverse)
library(mlr)
library(SHAPforxgboost)
library(iml)
library(featureImportance)

ada_check = FALSE
if(ada_check){
  path = ""
} else{
  path ="C:/R - Workspace/IML"
}
#get the model & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))

#------------------------------------------
#Shap Feature Importance
set.seed(123)

#Get shap values for each observation:
shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
#Sort variables by name, such that grouping categories is easier:
shap_values$shap_score = shap_values$shap_score %>% select(sort(current_vars()))

#Group together effects of categorical variables
list_categories = c(c("MSZoning", "MSZoning.C..all.", "MSZoning.RM"),
                    c("LotShape","LotShape.IR1","LotShape.Reg"),
                    c("BldgType","BldgType.1Fam","BldgType.TwnhsE"),
                    #c("BsmtFinType1","BsmtFinType1.ALQ","BsmtFinType1.Unf"),
                    #c("BsmtQual","BsmtQual.Ex","BsmtQual.TA"),
                    #c("Electrical","Electrical.Fuse.or.Mix","Electrical.SBrkr"),
                    #c("ExterCond","ExterCond.1","ExterCond.2"),
                    #c("ExterQual","ExterQual.Ex","ExterQual.TA"),
                    #c("FireplaceQu","FireplaceQu.AA","FireplaceQu.TA"),
                    c("Foundation","Foundation.BrkTil","Foundation.PConc"),
                    c("Functional","Functional.Min","Functional.Typ"),
                   # c("HeatingQC","HeatingQC.Ex","HeatingQC.TA"),
                    #c("KitchenQual","KitchenQual.Ex","KitchenQual.TA"),
                    c("Neighborhood","Neighborhood.Blmngtn","Neighborhood.Veenker"),
                    c("porch_type","porch_type.enclosed_porch","porch_type.wood_deck"),
                    c("RoofStyle","RoofStyle.Gable","RoofStyle.Hip"),
                    c("SaleCondition","SaleCondition.Abnorml","SaleCondition.Partial"),
                    c("SaleType","SaleType.COD","SaleType.WD"),
                    c("SeasonSold","SeasonSold.a","SeasonSold.su")
                    )
for(i in seq(from=1, to=length(list_categories), by=3)){
  name = list_categories[i]
  from =  list_categories[i+1]
  to =  list_categories[i+2]
  shap_values$shap_score[[name]] = shap_values$shap_score %>% select(from: to) %>% rowSums()
  shap_values$shap_score = select(shap_values$shap_score,  -(from: to))
  
}
#Now calculate absolute means manually for our feature importance measure:
shap_values$mean_shap_score =  shap_values$shap_score %>% summarise_all(~ mean(abs(.)))

#shap_values$shap_score = shap_values$shap_score %>% select(sort(current_vars()))
#shap_values$mean_shap_score = shap_values$mean_shap_score %>% select(sort(current_vars()))

#Now get the means in correct form for plotting
plotting_data = as_tibble(shap_values$mean_shap_score) %>% pivot_longer(cols = everything())
plotting_data = plotting_data %>% mutate(name = ifelse(name == "Electrical.Fuse.or.Mix", "Electrical", name))
shap_ranks = plotting_data

p<-ggplot(data=plotting_data, aes(x=value, y=reorder(name, value))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Shap Value (abs.)") +
  ylab("Feature") +
  labs(title = "SHAP Feature Importance (xgboost)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  #geom_text(aes(label=abs_value), vjust=0, size=2)
p


ggsave(filename = paste0(path, "/results/SHAP_feature_importance.jpg"), plot = p, dpi = 450)

#----------------------------
#Shap summary

shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = select(training_data, - SalePrice))
#Do summary plot for top 6 variables 
options(scipen=10000)
p3 = shap.plot.summary.wrap1(model = xgmodel$learner.model, X =  select(training_data, - SalePrice), top_n = 6, dilute = 2) +
  labs(title = "SHAP Summary Plot")  + ylab("Shapley value") +  xlab("Features") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  
p3
ggsave(filename = paste0(path, "/results/SHAP_summary_3.jpg"), plot = p3, dpi = 450)

#------------------------------
#Permutation Feature Importance 

#First define evaluation measure
# Use RMSE instead of RMSLE, because through permutation negative values (especially later in regression) are possible
f = function(task, model, pred, feats, extra.args) {

  sqrt(sum(   ( pred$data$response - pred$data$truth  )^2) /length(pred$data$response)  )
}
meas = makeMeasure(id = "RMSE", minimize = TRUE,
            properties = c("regr", "response"), fun = f, extra.args = list())
set.seed(123)

#Need to define manually, which variables are only permutated together:
permutation_list = list(
  MSZoning = c("MSZoning.RL", "MSZoning.RM", "MSZoning.C..all.", "MSZoning.FV"),
  LotShape = c("LotShape.Reg", "LotShape.IR1"),
  Neighborhood = c("Neighborhood.CollgCr", "Neighborhood.Veenker", "Neighborhood.Crawfor", "Neighborhood.NoRidge",
                   "Neighborhood.Mitchel", "Neighborhood.Somerst", "Neighborhood.NWAmes", "Neighborhood.OldTown",
                   "Neighborhood.BrkSide", "Neighborhood.Sawyer", "Neighborhood.NridgHt", "Neighborhood.NAmes",
                   "Neighborhood.SawyerW", "Neighborhood.IDOTRR", "Neighborhood.MeadowV", "Neighborhood.Edwards",
                   "Neighborhood.Timber", "Neighborhood.Gilbert", "Neighborhood.StoneBr", "Neighborhood.ClearCr",
                   "Neighborhood.NPkVill", "Neighborhood.Blmngtn", "Neighborhood.BrDale", "Neighborhood.SWISU"),
  numb_add_flr = "numb_add_flr",
  pool = "pool",
  BldgType = c("BldgType.1Fam", "BldgType.2fmCon", "BldgType.Duplex", "BldgType.TwnhsE"),
  RoofStyle = c("RoofStyle.Gable", "RoofStyle.Hip"),
  ExterQual = "ExterQual",
  ExterCond = "ExterCond",
  Foundation = c("Foundation.PConc", "Foundation.CBlock", "Foundation.BrkTil"),
  BsmtQual = "BsmtQual",
  BsmtFinType1 = "BsmtFinType1",
  HeatingQC = "HeatingQC",
  Electrical = "Electrical.Fuse.or.Mix",
  KitchenQual = "KitchenQual",
  Functional = c("Functional.Typ", "Functional.Min" ),
  FireplaceQu = "FireplaceQu" ,
  SaleType = c("SaleType.WD", "SaleType.New", "SaleType.COD"),
  SaleCondition = c("SaleCondition.Normal", "SaleCondition.Abnorml" ,"SaleCondition.Partial", "SaleCondition.Family"),
  SeasonSold = c("SeasonSold.sp", "SeasonSold.su", "SeasonSold.a"),
  porch_type = c("porch_type.enclosed_porch", "porch_type.multiple", "porch_type.open_porch", 
                 "porch_type.screen_porch", "porch_type.three_s_porch", "porch_type.wood_deck"),
  LotArea = "LotArea",
  YearBuilt = "YearBuilt",
  TotalBsmtSF = "TotalBsmtSF",
  GrLivArea = "GrLivArea",
  porch_area = "porch_area",
  FullBath = "FullBath",
  HalfBath = "HalfBath",
  BedroomAbvGr = "BedroomAbvGr",
  KitchenAbvGr = "KitchenAbvGr",
  GarageCars = "GarageCars",
  OverallQual = "OverallQual",
  OverallCond = "OverallCond",
  CentralAir = "CentralAir",
  PavedDrive = "PavedDrive",
  Remod = "Remod")
imp = featureImportance(xgmodel, data = training_data, n.feat.perm = 50, measures  = meas,
                        features = permutation_list
                        )
plotting_data = summary(imp)
permutation_ranks = plotting_data

p3<-ggplot(data=plotting_data, aes(x=RMSE, y=reorder(features, RMSE))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Feature Importance (RMSE)") +
  ylab("Feature") +
  labs(title = "Permutation Feature Importance (xgboost)") +
  scale_x_continuous(breaks= seq(0, 25000, by= 5000)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
#geom_text(aes(label=abs_value), vjust=0, size=2)
p3
ggsave(filename = paste0(path, "/results/permutation_feature_importance.jpg"), plot = p3, dpi = 450)

#Difference between Permutation & Shap rankings
ranks = left_join(permutation_ranks %>% rename(name = features), shap_ranks, by="name")
cor(ranks$RMSE, ranks$value, method = "spearman")
#0.9492921



#For Comparison with RMSLE:
f = function(task, model, pred, feats, extra.args) {
  
  sqrt(sum(   ( log(pred$data$response + 1) - log(pred$data$truth + 1) )^2) /length(pred$data$response)  )
}
meas = makeMeasure(id = "RMSLE", minimize = TRUE,
                   properties = c("regr", "response"), fun = f, extra.args = list())
set.seed(123)
imp = featureImportance(xgmodel, data = training_data, n.feat.perm = 50, measures  = meas,
                        features = permutation_list
)
plotting_data2 = summary(imp)

pp3<-ggplot(data=plotting_data2, aes(x=RMSLE, y=reorder(features, RMSLE))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Feature Importance (RMSLE)") +
  ylab("Feature") +
  labs(title = "Permutation Feature Importance") +
  #scale_x_continuous(breaks= seq(0, 25000, by= 5000)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
#geom_text(aes(label=abs_value), vjust=0, size=2)
pp3

#--------------------------------
#Interaction strength - H -statistic

#Warning: might have to reload packages afterwards, because it screws with some of the dependencies

#For all variables:
mod <- Predictor$new(xgmodel, data = select(training_data, - SalePrice), y = select(training_data, SalePrice))
set.seed(123)
ia <- Interaction$new(mod, grid.size = 50)
plot(ia)

#For GrLivArea specifically:
set.seed(123)
ia <- Interaction$new(mod, grid.size = 30, feature = "GrLivArea")
plot(ia)

#For OverallQual specifically:
set.seed(123)
ia <- Interaction$new(mod, grid.size = 30, feature = "OverallQual")
plot(ia)

#Group the effect of the dummies for each category together?

#If yes, then:

#The interaction strength between two features is the proportion of the variance of 
#the 2-dimensional partial dependence function that is not explained by 
#the sum of the two1-dimensional partial dependence functions.

#---> therefore, if we are interested in the average effect of a category, calculating the mean "should" be correct
# Then this effect would describe the average ratio of the explained variance by the interaction 
#But it doesnt take into account, that some categories might have stronger effects
#(We could, if we want to, weight them by e.g. some feature importance measure to account for that)


#---------------------------------
#Regression - Shap Feature Importance
lm = readRDS(paste0(path, "/results/linear_regression.rds"))
training_data_linear_reg = readRDS(paste0(path, "/results/train_linear_regression.rds"))

#Shap Feature Importance
#No package available there

#Permutation Feasure Importance  
f = function(truth, response) {
  sqrt(sum(   ( truth  - response )^2) /length(truth)  )
}
set.seed(123)
imp = featureImportance(object = lm, data = training_data_linear_reg, n.feat.perm = 30, measures  = c("RMSE" =  f), target="SalePrice",
                        features = list(
                          MSZoning = c("MSZoning.RL", "MSZoning.RM", "MSZoning.C..all.", "MSZoning.FV"),
                          LotShape = c("LotShape.Reg", "LotShape.IR1"),
                          Neighborhood = c("Neighborhood.CollgCr", "Neighborhood.Veenker", "Neighborhood.Crawfor", "Neighborhood.NoRidge",
                                           "Neighborhood.Mitchel", "Neighborhood.Somerst", "Neighborhood.NWAmes", "Neighborhood.OldTown",
                                           "Neighborhood.BrkSide", "Neighborhood.Sawyer", "Neighborhood.NridgHt", "Neighborhood.NAmes",
                                           "Neighborhood.SawyerW", "Neighborhood.IDOTRR", "Neighborhood.MeadowV", "Neighborhood.Edwards",
                                           "Neighborhood.Timber", "Neighborhood.Gilbert", "Neighborhood.StoneBr", "Neighborhood.ClearCr",
                                           "Neighborhood.NPkVill", "Neighborhood.Blmngtn", "Neighborhood.BrDale", "Neighborhood.SWISU"),
                          numb_add_flr = "numb_add_flr",
                          pool = "pool",
                          BldgType = c("BldgType.1Fam", "BldgType.2fmCon", "BldgType.Duplex", "BldgType.TwnhsE"),
                          RoofStyle = c("RoofStyle.Gable", "RoofStyle.Hip"),
                          ExterQual = "ExterQual",
                          ExterCond = "ExterCond",
                          Foundation = c("Foundation.PConc", "Foundation.CBlock", "Foundation.BrkTil"),
                          BsmtQual = "BsmtQual",
                          BsmtFinType1 = "BsmtFinType1",
                          HeatingQC = "HeatingQC",
                          Electrical = "Electrical.Fuse.or.Mix",
                          KitchenQual = "KitchenQual",
                          Functional = c("Functional.Typ", "Functional.Min" ),
                          FireplaceQu = "FireplaceQu" ,
                          SaleType = c("SaleType.WD", "SaleType.New", "SaleType.COD"),
                          SaleCondition = c("SaleCondition.Normal", "SaleCondition.Abnorml" ,"SaleCondition.Partial", "SaleCondition.Family"),
                          SeasonSold = c("SeasonSold.sp", "SeasonSold.su", "SeasonSold.a"),
                          porch_type = c("porch_type.enclosed_porch", "porch_type.multiple", "porch_type.open_porch", 
                                         "porch_type.screen_porch", "porch_type.three_s_porch", "porch_type.wood_deck"),
                          LotArea = "LotArea",
                          YearBuilt = "YearBuilt",
                          TotalBsmtSF = "TotalBsmtSF",
                          GrLivArea = "GrLivArea",
                          porch_area = "porch_area",
                          FullBath = "FullBath",
                          HalfBath = "HalfBath",
                          BedroomAbvGr = "BedroomAbvGr",
                          KitchenAbvGr = "KitchenAbvGr",
                          GarageCars = "GarageCars",
                          OverallQual = "OverallQual",
                          OverallCond = "OverallCond",
                          CentralAir = "CentralAir",
                          PavedDrive = "PavedDrive",
                          Remod = "Remod"
                        ))
plotting_data = summary(imp)

p4<-ggplot(data=plotting_data, aes(x=RMSE, y=reorder(features, RMSE))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Feature Importance (RMSE)") +
  ylab("Feature") +
  labs(title = "Permutation Feature Importance (linear regression)") +
  scale_x_continuous(breaks= seq(0, 25000, by= 5000)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
p4
ggsave(filename = paste0(path, "/results/permutation_feature_importance_linear_regression.jpg"), plot = p4, dpi = 450)

#T value Feature Importance  
plotting_data = enframe(summary(lm)[["coefficients"]][, "t value"]) %>% slice(2:n) %>% mutate(value = abs(value))

p5<-ggplot(data=plotting_data, aes(x=value, y=reorder(name, value))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("t-value Feature Importance") +
  ylab("Feature")
p5
ggsave(filename = paste0(path, "/results/t_value_feature_importance_linear_regression.jpg"), plot = p5)
#here we have the same problem again with dummies for our categories...

