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

#Group together effects of categorical variables
shap_values$shap_score$MSZoning =  shap_values$shap_score %>% select(MSZoning.RL: MSZoning.RH) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(MSZoning.RL: MSZoning.RH))

shap_values$shap_score$Neighborhood =  shap_values$shap_score %>% select(Neighborhood.CollgCr: Neighborhood.Blueste) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(Neighborhood.CollgCr: Neighborhood.Blueste))

shap_values$shap_score$LotShape =  shap_values$shap_score %>% select(LotShape.Reg: LotShape.IR2.5) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(LotShape.Reg: LotShape.IR2.5))

shap_values$shap_score$BldgType =  shap_values$shap_score %>% select(BldgType.1Fam: BldgType.Twnhs) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(BldgType.1Fam: BldgType.Twnhs))

shap_values$shap_score$RoofStyle =  shap_values$shap_score %>% select(RoofStyle.Gable: RoofStyle.Other) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(RoofStyle.Gable: RoofStyle.Other))

shap_values$shap_score$ExterQual =  shap_values$shap_score %>% select(ExterQual.Gd: ExterQual.Fa) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(ExterQual.Gd: ExterQual.Fa))

shap_values$shap_score$ExterQual =  shap_values$shap_score %>% select(ExterQual.Gd: ExterQual.Fa) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(ExterQual.Gd: ExterQual.Fa))

shap_values$shap_score$ExterCond =  shap_values$shap_score %>% select(ExterCond.2: ExterCond.1) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(ExterCond.2: ExterCond.1))

shap_values$shap_score$Foundation =  shap_values$shap_score %>% select(Foundation.PConc: Foundation.Other) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(Foundation.PConc: Foundation.Other))

shap_values$shap_score$BsmtQual =  shap_values$shap_score %>% select(BsmtQual.Gd: BsmtQual.No) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(BsmtQual.Gd: BsmtQual.No))

shap_values$shap_score$BsmtFinType1 =  shap_values$shap_score %>% select(BsmtFinType1.GLQ: BsmtFinType1.No) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(BsmtFinType1.GLQ: BsmtFinType1.No))

shap_values$shap_score$HeatingQ =  shap_values$shap_score %>% select(HeatingQC.Ex: HeatingQC.Po) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(HeatingQC.Ex: HeatingQC.Po))

shap_values$shap_score$Electrical =  shap_values$shap_score %>% select(Electrical.SBrkr: Electrical.Fuse.or.Mix) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(Electrical.SBrkr: Electrical.Fuse.or.Mix))

shap_values$shap_score$KitchenQual =  shap_values$shap_score %>% select(KitchenQual.Gd: KitchenQual.Fa) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(KitchenQual.Gd: KitchenQual.Fa))

shap_values$shap_score$Functional =  shap_values$shap_score %>% select(Functional.Typ: Functional.Maj) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(Functional.Typ: Functional.Maj))

shap_values$shap_score$FireplaceQu =  shap_values$shap_score %>% select(FireplaceQu.TA: FireplaceQu.No) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(FireplaceQu.TA: FireplaceQu.No))

shap_values$shap_score$SaleType =  shap_values$shap_score %>% select(SaleType.WD: SaleType.Other) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(SaleType.WD: SaleType.Other))

shap_values$shap_score$SaleCondition =  shap_values$shap_score %>% select(SaleCondition.Normal: SaleCondition.Family) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(SaleCondition.Normal: SaleCondition.Family))

shap_values$shap_score$SeasonSold =  shap_values$shap_score %>% select(SeasonSold.w: SeasonSold.a) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(SeasonSold.w: SeasonSold.a))

shap_values$shap_score$porch_type =  shap_values$shap_score %>% select(porch_type.enclosed_porch: porch_type.wood_deck) %>% rowSums()
shap_values$shap_score = select(shap_values$shap_score,  -(porch_type.enclosed_porch: porch_type.wood_deck))

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
ggsave(filename = paste0(path, "/results/SHAP_summary.jpg"), plot = p2)
#-----------------------------

