library(xgboost)
library(tidyverse)
library(mlr)
library(SHAPforxgboost)
library(iml)
library(featureImportance)

ada_check = FALSE

#Tell me if that works :)
if(ada_check){
  path = ""
  source(cleaning_path_train)
} else{
  path ="C:/R - Workspace/IML"
}

#get the model & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))

#------------------------------------------
#Shap Feature Importance
shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))

shap_values$shap_score = shap_values$shap_score %>% select(sort(current_vars()))


#Group together effects of categorical variables

list_categories = c(c("MSZoning", "MSZoning.C..all.", "MSZoning.RM"),
                    c("LotShape","LotShape.IR1","LotShape.Reg"),
                    c("BldgType","BldgType.1Fam","BldgType.TwnhsE"),
                    c("BsmtFinType1","BsmtFinType1.ALQ","BsmtFinType1.Unf"),
                    c("BsmtQual","BsmtQual.Ex","BsmtQual.TA"),
                    c("Electrical","Electrical.Fuse.or.Mix","Electrical.SBrkr"),
                    c("ExterCond","ExterCond.1","ExterCond.3"),
                    c("ExterQual","ExterQual.Ex","ExterQual.TA"),
                    c("FireplaceQu","FireplaceQu.AA","FireplaceQu.TA"),
                    c("Foundation","Foundation.BrkTil","Foundation.PConc"),
                    c("Functional","Functional.Maj","Functional.Typ"),
                    c("HeatingQC","HeatingQC.Ex","HeatingQC.TA"),
                    c("KitchenQual","KitchenQual.Ex","KitchenQual.TA"),
                    c("Neighborhood","Neighborhood.Blmngtn","Neighborhood.Veenker"),
                    c("porch_type","porch_type.enclosed_porch","porch_type.wood_deck"),
                    c("RoofStyle","RoofStyle.Gable","RoofStyle.Other"),
                    c("SaleCondition","SaleCondition.Abnorml","SaleCondition.Partial"),
                    c("SaleType","SaleType.COD","SaleType.WD"),
                    c("SeasonSold","SeasonSold.a","SeasonSold.w")
                    )

for(i in seq(from=1, to=length(list_categories), by=3)){
  name = list_categories[i]
  from =  list_categories[i+1]
  to =  list_categories[i+2]
  shap_values$shap_score[[name]] = shap_values$shap_score %>% select(from: to) %>% rowSums()
  shap_values$shap_score = select(shap_values$shap_score,  -(from: to))
  
}
shap_values$mean_shap_score =  shap_values$shap_score %>% summarise_all(~ mean(abs(.)))
shap_values$shap_score = shap_values$shap_score %>% select(sort(current_vars()))
shap_values$mean_shap_score = shap_values$mean_shap_score %>% select(sort(current_vars()))

plotting_data = as_tibble(shap_values$mean_shap_score) %>% pivot_longer(cols = everything()) %>% mutate(abs_value = abs(value))


p<-ggplot(data=plotting_data, aes(x=abs_value, y=reorder(name, abs_value))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Shap Value (abs.)") +
  ylab("Feature")
  #geom_text(aes(label=abs_value), vjust=0, size=2)
p
ggsave(filename = paste0(path, "/results/SHAP_feature_importance.jpg"), plot = p)

#----------------------------
#Shap summary
#Still need to figure out, how to deal with categories!
shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
shap_long <- shap.prep(shap_contrib = shap_values$shap_score, X_train = select(training_data, - SalePrice))
p2 = shap.plot.summary(shap_long)
p2
ggsave(filename = paste0(path, "/results/SHAP_summary.jpg"), plot = p2)

shap.plot.summary.wrap1(model = xgmodel$learner.model, X =  select(training_data, - SalePrice), top_n = 6, dilute = FALSE)

xgb.plot.shap(data = as.matrix(select(training_data, - SalePrice)), model = xgmodel$learner.model, top_n = 6)

#------------------------------
#Permutation Feature Importance   - NEW
f = function(task, model, pred, feats, extra.args) {

  sqrt(sum(   ( log(pred$data$response + 1) - log(pred$data$truth + 1) )^2) /length(pred$data$response)  )
}
meas = makeMeasure(id = "RMSLE", minimize = TRUE,
            properties = c("regr", "response"), fun = f, extra.args = list())

imp = featureImportance(xgmodel, data = training_data, n.feat.perm = 50, measures  = meas,
                        features = list(
                          MSZoning = c("MSZoning.RL", "MSZoning.RM", "MSZoning.C..all.", "MSZoning.FV", "MSZoning.RH"),
                          LotShape = c("LotShape.Reg", "LotShape.IR1", "LotShape.IR2.5"),
                          Neighborhood = c("Neighborhood.CollgCr", "Neighborhood.Veenker", "Neighborhood.Crawfor", "Neighborhood.NoRidge",
                                           "Neighborhood.Mitchel", "Neighborhood.Somerst", "Neighborhood.NWAmes", "Neighborhood.OldTown",
                                            "Neighborhood.BrkSide", "Neighborhood.Sawyer", "Neighborhood.NridgHt", "Neighborhood.NAmes",
                                           "Neighborhood.SawyerW", "Neighborhood.IDOTRR", "Neighborhood.MeadowV", "Neighborhood.Edwards",
                                           "Neighborhood.Timber", "Neighborhood.Gilbert", "Neighborhood.StoneBr", "Neighborhood.ClearCr",
                                           "Neighborhood.NPkVill", "Neighborhood.Blmngtn", "Neighborhood.BrDale", "Neighborhood.SWISU", "Neighborhood.Blueste"),
                          numb_add_flr = "numb_add_flr",
                          pool = "pool",
                          BldgType = c("BldgType.1Fam", "BldgType.2fmCon", "BldgType.Duplex", "BldgType.TwnhsE", "BldgType.Twnhs"),
                          RoofStyle = c("RoofStyle.Gable", "RoofStyle.Hip", "RoofStyle.Other"),
                          ExterQual = c("ExterQual.Gd", "ExterQual.TA", "ExterQual.Ex", "ExterQual.Fa"),
                          ExterCond = c("ExterCond.2", "ExterCond.3", "ExterCond.1"),
                          Foundation = c("Foundation.PConc", "Foundation.CBlock", "Foundation.BrkTil", "Foundation.Other"),
                          BsmtQual = c("BsmtQual.Gd", "BsmtQual.TA", "BsmtQual.Ex", "BsmtQual.Fa", "BsmtQual.No"),
                          BsmtFinType1 = c("BsmtFinType1.Unf", "BsmtFinType1.Rec", "BsmtFinType1.BLQ", "BsmtFinType1.LwQ", "BsmtFinType1.No"),
                          HeatingQC = c("HeatingQC.Ex", "HeatingQC.Gd", "HeatingQC.TA", "HeatingQC.Fa", "HeatingQC.Po"),
                          Electrical = c("Electrical.SBrkr", "Electrical.Fuse.or.Mix"),
                          KitchenQual = c("KitchenQual.Gd", "KitchenQual.TA", "KitchenQual.Ex", "KitchenQual.Fa"),
                          Functional = c("Functional.Typ", "Functional.Min", "Functional.Maj" ),
                          FireplaceQu = c("FireplaceQu.TA", "FireplaceQu.AA", "FireplaceQu.BA", "FireplaceQu.No"),
                          SaleType = c("SaleType.WD", "SaleType.New", "SaleType.COD", "SaleType.Other"),
                          SaleCondition = c("SaleCondition.Normal", "SaleCondition.Abnorml" ,"SaleCondition.Partial", "SaleCondition.Other", "SaleCondition.Family"),
                          SeasonSold = c("SeasonSold.w", "SeasonSold.sp", "SeasonSold.su", "SeasonSold.a"),
                          porch_type = c("porch_type.enclosed_porch", "porch_type.multiple", "porch_type.no", "porch_type.open_porch", 
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
                        )
)
plotting_data = summary(imp)


p3<-ggplot(data=plotting_data, aes(x=RMSLE, y=reorder(features, RMSLE))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Permutation Feature Importance (RMSLE)") +
  ylab("Feature")
#geom_text(aes(label=abs_value), vjust=0, size=2)
p3
ggsave(filename = paste0(path, "/results/permutation_feature_importance.jpg"), plot = p3)








#-----------------------------
#Permutation Feature Importance

# mod <- Predictor$new(xgmodel, data = select(training_data, - SalePrice), y = select(training_data, SalePrice))
# imp <- FeatureImp$new(predictor = mod, loss = "rmsle", compare="difference")
# imp$results = imp$results %>% arrange(feature)
# for(i in seq(from=1, to=length(list_categories), by=3)){
#   name = list_categories[i]
#   from =  list_categories[i+1]
#   to =  list_categories[i+2]
#   
#   index_from = which(imp$results$feature == from)
#   index_to = which(imp$results$feature == to)
#   
#   new_row = colSums( xxx[index_from:index_to,2:5] )
#   imp$results = imp$results[-c(index_from:index_to), ]
#   
#   imp$results = imp$results %>% add_row(feature = name, importance.05 = new_row[1], importance = new_row[2], importance.95 = new_row[3], permutation.error = new_row[4])
# }
# plotting_data2 = imp$results
# p3<-ggplot(data=plotting_data2, aes(x=importance, y=reorder(feature, importance))) +
#   geom_bar(stat="identity", fill="steelblue") +
#   xlab("Feature Importance (Loss: RMSLE)") +
#   ylab("Feature")
# p3
# ggsave(filename = paste0(path, "/results/permutation_feature_importance.jpg"), plot = p3)

#--------------------------------
#Interaction strength
mod <- Predictor$new(xgmodel, data = select(training_data, - SalePrice), y = select(training_data, SalePrice))
ia <- Interaction$new(mod,  grid.size = 50)



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
