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
  path ="C:/R - Workspace/IML"
}

#get the models & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
lm = readRDS(paste0(path, "/results/linear_regression.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))
mod <- Predictor$new(xgmodel, data = training_data)
mod_lm <- Predictor$new(lm, data = training_data)

#-------------------
#YearBuilt

#ALE
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
g1 = plot(eff) + geom_line(colour="darkred" , size = 0.7) +
    labs(title = "ALE plot of SalePrice using YearBuilt", subtitle="model = xgboost") +
   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1 
ggsave(filename = paste0(path, "/results/ALE_YearBuilt.jpg"), plot = g1, dpi = 450)

#Compare to linear regression ALE
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
g2 = plot(eff) + geom_line(colour="steelblue", linetype="twodash", size = 0.7) +
  labs(title = "Marginal effect on SalePrice using YearBuilt", subtitle="model = linear regression") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g2
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_lm.jpg"), plot = g2, dpi = 450)

#Combination ALE of xgboost and linear reg
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_xgboost <- eff$results #%>% mutate(.value = .value-max(.value))
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_lm<- eff$results #%>% mutate(.value = .value-max(.value))
g = ggplot() +
  geom_line(aes(y = eff.dat_lm$.value, x=eff.dat_lm$YearBuilt, color="steelblue"), linetype="twodash") + 
  geom_line(aes(y = eff.dat_xgboost$.value, x=eff.dat_xgboost$YearBuilt, color="darkred")) + # scale_x_reverse() +
  xlab("YearBuilt") + ylab("Marginal Effect (ALE)") +  #xlim(0, 1800) + ylim(-1000, 50000) +
  scale_color_identity(name = "Model",
                     breaks = c("darkred", "steelblue"),
                     labels = c("Xgboost", "Linear Regression"),
                     guide = "legend") +
  labs(title = "ALE plot of SalePrice using YearBuilt" , subtitle = "(both models)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_lm_xgboost.jpg"), plot = g, dpi = 450)

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
ggsave(filename = paste0(path, "/results/ALE_Remod.jpg"), plot = g, dpi = 450)

# eff <- FeatureEffect$new(mod, feature = "Remod", method="pdp", grid.size = 60)
# plot(eff)

#------------------------
#YearBuilt + Remod
eff <- FeatureEffect$new(mod, feature = c("YearBuilt", "Remod"), grid.size = 100, method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c(name = "ALE")  +
  labs(title = "ALE plot of SalePrice using Interaction of YearBuilt & Remod") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  #geom_point(data = training_data, aes(y =Remod, x = YearBuilt),color = "black", size = 1)
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_with_Remod.jpg"), plot = g, dpi = 450)

g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c(name = "ALE")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlim(2000, NA) 
g
ggsave(filename = paste0(path, "/results/ALE_YearBuilt_with_Remod_partial.jpg"), plot = g, dpi = 450)
training_data %>% filter(YearBuilt>2005) %>% count()
training_data %>% filter(YearBuilt==2006) %>% count()
training_data %>% filter(YearBuilt==2006 &Remod==1) %>% count()
training_data %>% filter(YearBuilt>2006) %>% count()
#Remeber than in linear regression, we would have to add an interaction effect into model, to get something like YearBuilt + Remod

#-------------------------
#YearBuilt Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'YearBuilt', y = 'YearBuilt', color_feature = as.factor('Remod')) + ggtitle("Shapley values for YearBuilt")
g1 = g1 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling") +geom_point(size=1) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g1$layers[[2]] = NULL
g1
#Might have to add the legend manually (i.e. add it to picture afterwards), because the ggplot legend doesnt work here
ggsave(filename = paste0(path, "/results/SHAP_dependence_YearBuilt_with_Remod.jpg"), plot = g1, dpi = 450)                                                                                                                    
                                                                                                                    
# g2 <- shap.plot.dependence(data_long = shap_long, x = 'YearBuilt', y = 'YearBuilt', color_feature = as.factor('Remod')) + ggtitle("SHAP values of YearBuilt")
# g2 = g2 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling")  + xlim(2000 ,NA) +geom_point(size=0.9)
# g2$layers[[2]] = NULL
# g2
# ggsave(filename = paste0(path, "/results/SHAP_dependence_YearBuilt_with_Remod_partial.jpg"), plot = g2)                                                                                                                           
  

#-------------------
#GrLivArea

#ALE
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 200)
g1 = plot(eff)  + geom_line(colour="darkred", size = 0.7) + #+ xlim(0, 3500)
  labs(title = "ALE plot of SalePrice using GrLivArea") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = paste0(path, "/results/ALE_GrLivArea.jpg"), plot = g1, dpi = 450) 
# g2 = plot(eff)  + geom_line(colour="darkred", size = 0.7) + xlim(500, 3000)
# g2
#-------------------
#OverallQual
#ALE
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale")
g1 = plot(eff)  + geom_line(colour="steelblue", size = 0.7) +
  labs(title = "ALE plot of SalePrice using OverallQual") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = paste0(path, "/results/ALE_OverallQual.jpg"), plot = g1, dpi = 450) 

#LotArea
#ALE
eff <- FeatureEffect$new(mod, feature = "LotArea", method="ale",  grid.size = 10)
g1 = plot(eff)  + geom_line(colour="darkgreen", size = 0.7) +
  labs(title = "ALE plot of SalePrice using LotArea") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = paste0(path, "/results/ALE_LotArea.jpg"), plot = g1, dpi = 450)

#-------------------
#GrLivArea + OverallQual 
#ALE
eff <- FeatureEffect$new(mod, feature = c("GrLivArea", "OverallQual"), method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
   scale_fill_viridis_c(name = "ALE")  +
  labs(title = "ALE plot of SalePrice using Interaction of GrLivArea & OverallQual") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g
ggsave(filename = paste0(path, "/results/ALE_GrLivArea_with_OverallQual.jpg"), plot = g, dpi = 450)


#Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "OverallQual" ) + 
  labs(title = "Shapley values for GrLivArea", subtitle="with OverallQual highlighted") +
  scale_color_gradient(low="red", high="green")
g1 = g1 + labs(color = "OverallQual") +geom_point(size=1) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g1$layers[[2]] = NULL
g1
ggsave(filename = paste0(path, "/results/SHAP_dependence_GrLivArea_with_OverallQual.jpg"), plot = g1, dpi = 450)

#Shap Dependence - highlight points
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))


training_data %>% filter( GrLivArea < 2000 & GrLivArea> 1600 & OverallQual > 8) 
training_data %>% filter( GrLivArea < 1700 & GrLivArea> 1680 & OverallQual < 5) 
point1 = shap_long %>% filter(rfvalue == 1698 , variable == "GrLivArea")
point2 = shap_long %>% filter(rfvalue == 1699 , variable == "GrLivArea")
points = bind_rows(point1, point2)
g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "OverallQual" ) + 
  labs(title = "Shapley values for GrLivArea", subtitle="with OverallQual & special points highlighted") +
  scale_color_gradient(low="red", high="green") +
  geom_point(size=1) + labs(color = "OverallQual") + geom_point(data=points, aes(x=rfvalue, y=value), colour="blue", size=2) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g1$layers[[2]] = NULL
g1
ggsave(filename = paste0(path, "/results/SHAP_dependence_GrLivArea_with_OverallQual_points_highlighted.jpg"), plot = g1, dpi = 450)

#These are the points: 
training_data %>% filter( GrLivArea ==1699  & OverallQual < 5) #ID is 1187
training_data %>% filter( GrLivArea ==1698  & OverallQual > 8) #ID is 725




#-------------------
#GrLivArea + LotArea 
#ALE
mod <- Predictor$new(xgmodel, data = filter(training_data, GrLivArea<= 4000 & LotArea<=100000))
eff <- FeatureEffect$new(mod, feature = c("GrLivArea", "LotArea"), method="ale")
g = plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
   scale_fill_viridis_c() +
  labs(title = "ALE plot of SalePrice using Interaction of GrLivArea & LotArea") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g
ggsave(filename = paste0(path, "/results/ALE_GrLivArea_with_LotArea.jpg"), plot = g, dpi = 450)
mod <- Predictor$new(xgmodel, data = filter(training_data))

#Shap Dependence
# shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
# g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "LotArea" ) + 
#     labs(title = "Shapley values for GrLivArea", subtitle="with LotArea highlighted") +
#     scale_colour_gradient2(low="green", mid="red", high="black", midpoint=3000,
#                                                                limits = c(min(training_data$LotArea), max(training_data$LotArea)), ) + 
#    labs(color = "LotArea") +geom_point(size=1) +
#   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
# g1$layers[[2]] = NULL
# g1
# ggsave(filename = paste0(path, "/results/SHAP_dependence_GrLivArea_with_LotArea.jpg"), plot = g1, dpi = 450)


#--------------
#TotalBsmtSF

#PDP
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
g = plot(eff)  + geom_line(colour="steelblue", linetype="twodash", size = 0.7) + ylab("Predicted Sale Price") +
  xlim(0, 1800) + ylim(165000, 225000) +
  labs(title = "PDP of SalePrice using TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = paste0(path, "/results/PDP_TotalBsmtSF.jpg"), plot = g, dpi = 450)

#ALE
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
g = plot(eff) + geom_line(colour="darkred", size = 0.7)  + ylab("ALE of SalePrice") + xlim(0, 1800) + ylim(-18000, 42000) +
  labs(title = "ALE plot of SalePrice using TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = paste0(path, "/results/ALE_TotalBsmtSF.jpg"), plot = g, dpi = 450)

#Combination
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() + 
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$TotalBsmtSF), color="steelblue", linetype="twodash") + 
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$TotalBsmtSF), color="darkred") +
  xlim(0, 1800) + ylim(-1000, 50000) + xlab("TotalBsmtSF") + ylab("Marginal Effect") +
  labs(title = "Marginal effect of ALE and PDP using TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = paste0(path, "/results/Combination_ALE_PDP_TotalBsmtSF.jpg"), plot = g, dpi = 450)



















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



