library(xgboost)
library(tidyverse)
library(mlr)
library(SHAPforxgboost)
library(iml)




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
#-----------------------------
#Permutatuon Feature Importance

mod <- Predictor$new(xgmodel, data = select(training_data, - SalePrice), y = select(training_data, SalePrice))
imp <- FeatureImp$new(predictor = mod, loss = "rmsle", compare="difference")
imp$results = imp$results %>% arrange(feature)
for(i in seq(from=1, to=length(list_categories), by=3)){
  name = list_categories[i]
  from =  list_categories[i+1]
  to =  list_categories[i+2]
  
  index_from = which(imp$results$feature == from)
  index_to = which(imp$results$feature == to)
  
  new_row = colSums( xxx[index_from:index_to,2:5] )
  imp$results = imp$results[-c(index_from:index_to), ]
  
  imp$results = imp$results %>% add_row(feature = name, importance.05 = new_row[1], importance = new_row[2], importance.95 = new_row[3], permutation.error = new_row[4])
}
plotting_data2 = imp$results
p3<-ggplot(data=plotting_data2, aes(x=importance, y=reorder(feature, importance))) +
  geom_bar(stat="identity", fill="steelblue") +
  xlab("Feature Importance (Loss: RMSLE)") +
  ylab("Feature")
p3
ggsave(filename = paste0(path, "/results/permutation_feature_importance.jpg"), plot = p3)



