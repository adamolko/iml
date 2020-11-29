library(iml)
library(xgboost)
library(tidyverse)
library(SHAPforxgboost)
library(mlr)
library(viridis)

ada_check = FALSE

if(ada_check){
  path = ""
  source(cleaning_path_train)
} else{
  path ="D:/R - Workspace/IML"
}

#get the model & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
lm = readRDS(paste0(path, "/results/linear_regression.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))

mod <- Predictor$new(xgmodel, data = training_data)
mod_lm <- Predictor$new(lm, data = training_data)

#-------------------
#YearBuilt

#ALE
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
g1 = plot(eff) + geom_line(colour="steelblue" , linetype="twodash", size = 0.7)
g1 
ggsave(filename = paste0(path, "/results/ALE_YearBuilt.jpg"), plot = g1)

#Need to compare linear regression
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
g2 = plot(eff) + geom_line(colour="darkred" , size = 0.7)
g2
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_lm.jpg"), plot = g2)
#Combination ALE of xgboost and linear reg
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_xgboost <- eff$results #%>% mutate(.value = .value-max(.value))
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_lm<- eff$results #%>% mutate(.value = .value-max(.value))
g = ggplot() + 
  geom_line(aes(y = eff.dat_lm$.value, x=eff.dat_lm$YearBuilt, color = "darkred")) + 
  geom_line(aes(y = eff.dat_xgboost$.value, x=eff.dat_xgboost$YearBuilt, color="steelblue"), linetype="twodash") + # scale_x_reverse() +
  xlab("YearBuilt") + ylab("Marginal Effect (ALE)") +  #xlim(0, 1800) + ylim(-1000, 50000) +
  scale_color_identity(name = "Model",
                     breaks = c("darkred", "steelblue"),
                     labels = c("Linear Regression", "Xgboost"),
                     guide = "legend")
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_lm_xgboost.jpg"), plot = g)

#PDP
# eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="pdp", grid.size = 20)
# g = plot(eff) + geom_line(colour="darkred", size = 0.7)
# g
# ggsave(filename = paste0(path, "/results/PDP_YearBuilt.jpg"), plot = g)

#Combination of ALE & PDP
# eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="pdp", grid.size = 50)
# eff.dat_pdp <- eff$results %>% mutate(.value = .value-max(.value))
# eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 50)
# eff.dat_ale<- eff$results %>% mutate(.value = .value-max(.value))
# g = ggplot() + 
#   geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$YearBuilt), color = "darkred") + 
#   geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$YearBuilt), color="steelblue", linetype="twodash") +
#   scale_x_reverse() +
#    xlab("YearBuilt") + ylab("Marginal Effect") #xlim(0, 1800) + ylim(-1000, 50000) +
# g
# ggsave(filename = paste0(path, "/results/Combination_ALE_PDP_YearBuilt.jpg"), plot = g)



#------------------------
#Remod
eff <- FeatureEffect$new(mod, feature = "Remod", method="ale", grid.size = 100)
g = plot(eff)
g
ggsave(filename = paste0(path, "/results/ALE_Remod"), plot = g)

eff <- FeatureEffect$new(mod, feature = "Remod", method="pdp", grid.size = 60)
plot(eff)

#------------------------
#YearBuilt + Remod
eff <- FeatureEffect$new(mod, feature = c("YearBuilt", "Remod"), grid.size = 100, method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c() + 
  geom_point(data = training_data, aes(y =Remod, x = YearBuilt),color = "black", size = 1)
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_with_Remod.jpg"), plot = g)
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c() + 
  xlim(2000, NA) +
  geom_point(data = training_data, aes(y =Remod, x = YearBuilt),color = "black", size = 1)
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_with_Remod_partial.jpg"), plot = g)
training_data %>% filter(YearBuilt>2005) %>% count()
training_data %>% filter(YearBuilt==2006) %>% count()
training_data %>% filter(YearBuilt==2006 &Remod==1) %>% count()
training_data %>% filter(YearBuilt>2006) %>% count()
#Remeber than in linear regression, we would have to add an interaction effect into model, to get something like YearBuilt + Remod

#-------------------------
#YearBuilt Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'YearBuilt', y = 'YearBuilt', color_feature = as.factor('Remod')) + ggtitle("SHAP values of YearBuilt")
g1 = g1 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling") +geom_point(size=0.8)
g1$layers[[2]] = NULL
g1

ggsave(filename = paste0(path, "/results/SHAP_dependence_YearBuilt_with_Remod.jpg"), plot = g1)                                                                                                                    
                                                                                                                    
g2 <- shap.plot.dependence(data_long = shap_long, x = 'YearBuilt', y = 'YearBuilt', color_feature = as.factor('Remod')) + ggtitle("SHAP values of YearBuilt")
g2 = g2 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling")  + xlim(2000 ,NA) +geom_point(size=0.9)
g2$layers[[2]] = NULL
g2
ggsave(filename = paste0(path, "/results/SHAP_dependence_YearBuilt_with_Remod_partial.jpg"), plot = g2)                                                                                                                           
  

#-------------------
#GrLivArea

#ALE
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 200)
g1 = plot(eff)  + geom_line(colour="darkred", size = 0.7) #+ xlim(0, 3500)
g1
ggsave(filename = paste0(path, "/results/ALE_GrLivArea.jpg"), plot = g1) 
g2 = plot(eff)  + geom_line(colour="darkred", size = 0.7) + xlim(500, 3000)
g2
#-------------------
#OverallQual
#ALE
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale")
g1 = plot(eff)  + geom_line(colour="darkred", size = 0.7)
g1
ggsave(filename = paste0(path, "/results/ALE_OverallQual.jpg"), plot = g1) 

#LotArea
#ALE
eff <- FeatureEffect$new(mod, feature = "LotArea", method="ale",  grid.size = 10)
g1 = plot(eff)  + geom_line(colour="darkred", size = 0.7)
g1
ggsave(filename = paste0(path, "/results/ALE_LotArea.jpg"), plot = g1) 

#-------------------
#GrLivArea + OverallQual 
#ALE
eff <- FeatureEffect$new(mod, feature = c("GrLivArea", "OverallQual"), method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c() 
g
ggsave(filename = paste0(path, "/results/ALE_GrLivArea_with_OverallQual.jpg"), plot = g)


#Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "OverallQual" ) + 
  ggtitle("SHAP values of YearBuilt")  +scale_color_gradient(low="red", high="green")
g1 = g1 + labs(color = "OverallQual") +geom_point(size=1)
g1$layers[[2]] = NULL
g1
ggsave(filename = paste0(path, "/results/SHAP_dependence_GrLivArea_with_OverallQual.jpg"), plot = g1)

#-------------------
#GrLivArea + LotArea 
#ALE
mod <- Predictor$new(xgmodel, data = filter(training_data, GrLivArea<= 4000 & LotArea<=100000))
eff <- FeatureEffect$new(mod, feature = c("GrLivArea", "LotArea"), method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c() 
g
ggsave(filename = paste0(path, "/results/ALE_GrLivArea_with_LotArea.jpg"), plot = g)
mod <- Predictor$new(xgmodel, data = filter(training_data))

#Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "LotArea" ) + 
  ggtitle("SHAP values of YearBuilt")  +scale_colour_gradient2(low="green", mid="red", high="black", midpoint=3000,
                                                               limits = c(min(training_data$LotArea), max(training_data$LotArea)), ) + 
   labs(color = "LotArea") +geom_point(size=1)
g1$layers[[2]] = NULL
g1
ggsave(filename = paste0(path, "/results/SHAP_dependence_GrLivArea_with_LotArea.jpg"), plot = g1)

#-------------------
#LotArea

#ALE
eff <- FeatureEffect$new(mod, feature = "LotArea", method="ale", grid.size = 30)
eff$plot() + xlim(0, 30000)

#PDP
eff <- FeatureEffect$new(mod, feature = "LotArea", method="pdp", grid.size = 100)
eff$plot()

#--------------
#TotalBsmtSF

#PDP
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
g = plot(eff)  + geom_line(colour="darkred", size = 0.7) + ylab("Predicted Sale Price")  +xlim(0, 1800) + ylim(165000, 225000)
g
ggsave(filename = paste0(path, "/results/PDP_TotalBsmtSF.jpg"), plot = g)

#ALE
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
g = plot(eff) + geom_line(colour="steelblue", linetype="twodash", size = 0.7)  + ylab("ALE of SalePrice") + xlim(0, 1800) + ylim(-18000, 42000)
g
ggsave(filename = paste0(path, "/results/ALE_TotalBsmtSF.jpg"), plot = g)

#Combination
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() + 
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$TotalBsmtSF), color = "darkred") + 
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$TotalBsmtSF), color="steelblue", linetype="twodash") +
  xlim(0, 1800) + ylim(-1000, 50000) + xlab("TotalBsmtSF") + ylab("Marginal Effect")
g
ggsave(filename = paste0(path, "/results/Combination_ALE_PDP_TotalBsmtSF.jpg"), plot = g)

#-----------------
#Shap Dependence Tests

#TotalBsmtSF
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'TotalBsmtSF', y = 'TotalBsmtSF') +#, color_feature = as.factor('Remod')) + 
  ggtitle("SHAP values of YearBuilt")
g1 = g1 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling") +geom_point(size=0.8)
g1$layers[[2]] = NULL
g1

#OverallCond
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'OverallQual', y = 'OverallQual') +#, color_feature = as.factor('Remod')) + 
  ggtitle("SHAP values of YearBuilt")
g1 = g1 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling") +geom_point(size=0.8)
g1$layers[[2]] = NULL
g1




#--------------
#Other variables

#PDP
eff <- FeatureEffect$new(mod, feature = "BedroomAbvGr", method="pdp", grid.size = 100)
eff$plot()

#ALE
eff <- FeatureEffect$new(mod, feature = "BedroomAbvGr", method="ale", grid.size = 100)
eff$plot() 

#PDP
eff <- FeatureEffect$new(mod, feature = "FullBath", method="pdp", grid.size = 100)
eff$plot()

#ALE
eff <- FeatureEffect$new(mod, feature = "FullBath", method="ale", grid.size = 100)
eff$plot() 

#PDP
eff <- FeatureEffect$new(mod, feature = "GarageCars", method="pdp", grid.size = 100)
eff$plot()

#ALE
eff <- FeatureEffect$new(mod, feature = "GarageCars", method="ale", grid.size = 100)
eff$plot() 

#PDP
eff <- FeatureEffect$new(mod, feature = "numb_add_flr", method="pdp", grid.size = 100)
eff$plot()

#ALE
eff <- FeatureEffect$new(mod, feature = "numb_add_flr", method="ale", grid.size = 100)
eff$plot() 

#PDP
eff <- FeatureEffect$new(mod, feature = "KitchenAbvGr", method="pdp", grid.size = 100)
eff$plot()

#ALE
eff <- FeatureEffect$new(mod, feature = "KitchenAbvGr", method="ale", grid.size = 100)
eff$plot() 



