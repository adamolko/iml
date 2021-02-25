library(iml)
library(xgboost)
library(tidyverse)
library(SHAPforxgboost)
library(mlr)
library(viridis)

#get the models & the training data first
xgmodel = readRDS("results/xgboost_model.rds")
lm = readRDS("results/linear_regression.rds")
training_data = readRDS("results/training_data.rds")
mod <- Predictor$new(xgmodel, data = training_data)
mod_lm <- Predictor$new(lm, data = training_data)

#-------------------
#YearBuilt

#ALE
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
g1 = plot(eff) + geom_line(colour="darkred" , size = 0.7) +
    labs(title = "ALE plot of SalePrice using YearBuilt", subtitle="grid size = 20") +
   theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1 
ggsave(filename = "results/ALE_YearBuilt.jpg", plot = g1, dpi = 450, width = 9, height = 6)

#Compare to linear regression ALE
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
g2 = plot(eff) + geom_line(colour="steelblue", linetype="twodash", size = 0.7) +
  labs(title = "Marginal effect on SalePrice using YearBuilt", subtitle="model = linear regression") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g2
ggsave(filename = "results/ALE_YearBuilt_lm.jpg", plot = g2, dpi = 450)

#Combination ALE of xgboost and linear reg
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_xgboost <- eff$results 
eff <- FeatureEffect$new(mod_lm, feature = "YearBuilt", method="ale", grid.size = 20)
eff.dat_lm<- eff$results 
g = ggplot() +
  geom_line(aes(y = eff.dat_lm$.value, x=eff.dat_lm$YearBuilt, color="steelblue"), linetype="twodash") + 
  geom_line(aes(y = eff.dat_xgboost$.value, x=eff.dat_xgboost$YearBuilt, color="darkred")) +
  xlab("YearBuilt") + ylab("Marginal Effect (ALE)") + 
  scale_color_identity(name = "Model",
                     breaks = c("darkred", "steelblue"),
                     labels = c("Xgboost", "Linear Regression"),
                     guide = "legend") +
  labs(title = "ALE plot of SalePrice using YearBuilt" , subtitle = "both models, grid size = 20") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/ALE_YearBuilt_lm_xgboost.jpg", plot = g, dpi = 450, width = 10, height = 6)


#Combination of ALE & PDP
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="pdp", grid.size = 50)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 50)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() +
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$YearBuilt, color = "darkred")) +
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$YearBuilt, color="steelblue"), linetype="twodash") +
  labs(title = "Marginal effect of YearBuilt") +  xlab("YearBuilt") + ylab("Marginal Effect") +
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("ALE", "PD"),
                       guide = "legend") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
   xlab("YearBuilt") + ylab("Marginal Effect") 
g
ggsave(filename = "results/Combination_ALE_PDP_YearBuilt.jpg", plot = g)

#------------------------
#Remod
eff <- FeatureEffect$new(mod, feature = "Remod", method="ale", grid.size = 20)
g = plot(eff) + scale_x_continuous(breaks= seq(0, 1, by= 1)) + geom_line(colour="darkred", size = 0.7) + #+ xlim(0, 3500)
  labs(title = "ALE plot of SalePrice using Remod", subtitle = "grid size = 20") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g
ggsave(filename = "results/ALE_Remod.jpg", plot = g, dpi = 450, width = 9, height = 6)

#------------------------
#YearBuilt + Remod
eff <- FeatureEffect$new(mod, feature = c("YearBuilt", "Remod"), grid.size = 20, method="ale")
g = plot(eff) + 
  ggtitle("ALE") + scale_fill_viridis_c(name = "ALE")  +
  labs(title = "ALE of SalePrice", subtitle = "grid size = 20" ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks= seq(0, 1, by= 1)) +
  geom_point(data = training_data, aes(y =Remod, x = YearBuilt),color = "black", size = 1)
g
ggsave(filename = "results/ALE_YearBuilt_with_Remod.jpg", plot = g, dpi = 450, width = 9, height = 6)

g = plot(eff) + 
  labs(title = "ALE of SalePrice", subtitle = "grid size = 20" )  + scale_fill_viridis_c(name = "ALE")  +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
 scale_y_continuous(breaks= seq(0, 1, by= 1)) + scale_x_continuous(breaks= seq(2000, 2012, by= 2), limits = c(2000,NA))
g
ggsave(filename = "results/ALE_YearBuilt_with_Remod_partial.jpg", plot = g, dpi = 450, width = 9, height = 6)
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

ggsave(filename = "results/SHAP_dependence_YearBuilt_with_Remod.jpg", plot = g1, dpi = 450)

#-------------------
#GrLivArea

#ALE
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 80)
g1 = plot(eff)  + geom_line(colour="darkred", size = 0.7) + 
  labs(title = "ALE plot of SalePrice using GrLivArea", subtitle = "grid size = 80") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = "results/ALE_GrLivArea.jpg", plot = g1, dpi = 450, width = 8, height = 6) 

#Combination ALE & PDP
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="pdp", grid.size = 30)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 30)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() +
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$GrLivArea, color = "darkred")) +
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$GrLivArea, color="steelblue"), linetype="twodash") +
  labs(title = "Marginal effect of GrLivArea") + xlab("GrLivArea") + ylab("Marginal Effect") +
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("ALE", "PD"),
                       guide = "legend") +
  geom_rug(alpha = 0.2, position = "jitter", sides="b", aes(y =  training_data$GrLivArea, x= training_data$GrLivArea)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  xlab("GrLivArea") + ylab("Marginal Effect")
g
ggsave(filename = "results/Combination_ALE_PDP_GrLivArea.jpg", plot = g, dpi = 450)

#Compare to linear regression ALE
eff <- FeatureEffect$new(mod_lm, feature = "GrLivArea", method="ale", grid.size = 80)
g2 = plot(eff) + geom_line(colour="steelblue", linetype="twodash", size = 0.7) +
  labs(title = "Marginal effect on SalePrice using YearBuilt", subtitle="model = linear regression") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g2

#Combination ALE of xgboost and linear reg
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 80)
eff.dat_xgboost <- eff$results 
eff <- FeatureEffect$new(mod_lm, feature = "GrLivArea", method="ale", grid.size = 80)
eff.dat_lm<- eff$results 
g = ggplot() +
  geom_line(aes(y = eff.dat_lm$.value, x=eff.dat_lm$GrLivArea, color="steelblue"), linetype="twodash") + 
  geom_line(aes(y = eff.dat_xgboost$.value, x=eff.dat_xgboost$GrLivArea, color="darkred")) + 
  xlab("GrLivArea") + ylab("Marginal Effect (ALE)") + 
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("Xgboost", "Linear Regression"),
                       guide = "legend") +
  labs(title = "ALE plot of SalePrice using GrLivArea" , subtitle = "both models, grid size = 80") + 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/Combination_ALE_GrLivArea_xgboost_lm.jpg", plot = g, dpi = 450, width = 8, height = 6) 

#-------------------
#OverallQual
#ALE
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale", grid.size = 20)
g1 = plot(eff)  + geom_line(colour="steelblue", size = 0.7) +
  labs(title = "ALE plot of SalePrice using OverallQual", subtitle = "grid size = 20") +
  scale_x_continuous(breaks= seq(1, 10, by= 1)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = "results/ALE_OverallQual.jpg", plot = g1, dpi = 450, width = 8, height = 5)
ggsave(filename = "results/ALE_OverallQual_version_2.jpg", plot = g1, dpi = 450, width = 8, height = 6)
#ALE as category
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale", grid.size = 20)
eff$feature.type = "categorical"
g1 = plot(eff)  + 
  scale_x_continuous(breaks= seq(1, 10, by= 1)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = "results/ALE_OverallQual_Categorical.jpg", plot = g1, dpi = 450, width = 8, height = 5)

#ALE + PDP
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="pdp", grid.size = 20)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale", grid.size = 20)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() +
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$OverallQual, color = "darkred")) +
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$OverallQual, color="steelblue"), linetype="twodash") +
  labs(title = "Marginal effect of OverallQual") +   xlab("OverallQual") + ylab("Marginal Effect") +
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("ALE", "PD"),
                       guide = "legend") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  xlab("OverallQual") + ylab("Marginal Effect") 
g
ggsave(filename = "results/Combination_ALE_PDP_OverallQual.jpg", plot = g, dpi = 450)

#SHAP Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'OverallQual', y = 'OverallQual') + 
  labs(title = "Shapley values for OverallQual") +
  scale_color_gradient(low="red", high="green") +
  scale_x_continuous(breaks= seq(1, 10, by= 1)) 
g1 = g1 + labs(color = "OverallQual") +geom_point(size=1) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g1$layers[[2]] = NULL
g1
ggsave(filename = "results/SHAP_dependence_OverallQual.jpg", plot = g1,dpi = 450, width = 10, height = 6)

#Combination ALE of xgboost and linear reg
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale", grid.size = 20)
eff.dat_xgboost <- eff$results 
eff <- FeatureEffect$new(mod_lm, feature = "OverallQual", method="ale", grid.size = 20)
eff.dat_lm<- eff$results 
g = ggplot() +
  geom_line(aes(y = eff.dat_lm$.value, x=eff.dat_lm$OverallQual, color="steelblue"), linetype="twodash") + 
  geom_line(aes(y = eff.dat_xgboost$.value, x=eff.dat_xgboost$OverallQual, color="darkred")) + 
  xlab("OverallQual") + ylab("Marginal Effect (ALE)") +  
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("Xgboost", "Linear Regression"),
                       guide = "legend") +
  labs(title = "ALE plot of SalePrice using OverallQual" , subtitle = "both models, grid size = 20") + 
  scale_x_continuous(breaks= seq(1, 10, by= 1)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/Combination_ALE_OverallQual_xgboost_lm.jpg", plot = g, dpi = 450, width = 8, height = 6) 


#LotArea
#ALE
eff <- FeatureEffect$new(mod, feature = "LotArea", method="ale",  grid.size = 10)
g1 = plot(eff)  + geom_line(colour="darkgreen", size = 0.7) +
  labs(title = "ALE plot of SalePrice using LotArea") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g1
ggsave(filename = "results/ALE_LotArea.jpg", plot = g1, dpi = 450)

#-------------------
#GrLivArea + OverallQual 
#ALE
eff <- FeatureEffect$new(mod, feature = c("GrLivArea", "OverallQual"), method="ale", grid.size = 20)
g = plot(eff) + 
   scale_fill_viridis_c(name = "ALE")  +
  labs(title = "ALE of SalePrice", subtitle = "grid size = 20") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks= seq(1, 10, by= 1))
g
ggsave(filename = "results/ALE_GrLivArea_with_OverallQual.jpg", plot = g, dpi = 450, width = 9, height = 6)


#Shap Dependence
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'GrLivArea', y = 'GrLivArea',  color_feature = "OverallQual" ) + 
  labs(title = "Shapley values for GrLivArea", subtitle="with OverallQual highlighted") +
  scale_color_gradient(low="red", high="green")
g1 = g1 + labs(color = "OverallQual") +geom_point(size=1) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
g1$layers[[2]] = NULL
g1
ggsave(filename = "results/SHAP_dependence_GrLivArea_with_OverallQual.jpg", plot = g1,dpi = 450, width = 10, height = 6)

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
ggsave(filename = "results/SHAP_dependence_GrLivArea_with_OverallQual_points_highlighted.jpg", plot = g1,dpi = 450, width = 10, height = 6)

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
ggsave(filename = "results/ALE_GrLivArea_with_LotArea.jpg", plot = g, dpi = 450)
mod <- Predictor$new(xgmodel, data = filter(training_data))


#--------------
#TotalBsmtSF

#PDP
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
g = plot(eff)  + geom_line(colour="steelblue", linetype="twodash", size = 0.7) + ylab("Predicted Sale Price") +
  xlim(0, 1800) + ylim(165000, 225000) +
  labs(title = "PDP of SalePrice using TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/PDP_TotalBsmtSF.jpg", plot = g, dpi = 450)

#ALE
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
g = plot(eff) + geom_line(colour="darkred", size = 0.7)  + ylab("ALE of SalePrice") + xlim(0, 1800) + ylim(-18000, 42000) +
  labs(title = "ALE plot of SalePrice using TotalBsmtSF") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/ALE_TotalBsmtSF.jpg", plot = g, dpi = 450)

#Combination
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 20)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 20)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
g = ggplot() + 
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$TotalBsmtSF, color="steelblue"), linetype="twodash") + 
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$TotalBsmtSF, color="darkred")) +
  xlim(0, 2000) + ylim(-1000, 50000) + xlab("TotalBsmtSF") + ylab("Marginal Effect") +
  labs(title = "Marginal effect of TotalBsmtSF", subtitle="grid size = 20") +
  scale_color_identity(name = "Model",
                       breaks = c("darkred", "steelblue"),
                       labels = c("ALE", "PD"),
                       guide = "legend") +
  geom_rug(alpha = 0.2, position = "jitter", sides="b", aes(y =  training_data$TotalBsmtSF, x= training_data$TotalBsmtSF)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
g
ggsave(filename = "results/Combination_ALE_PDP_TotalBsmtSF.jpg", plot = g, dpi = 450, width = 8, height = 6)


#-----------------
#Shap Dependence Tests

#TotalBsmtSF
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'TotalBsmtSF', y = 'TotalBsmtSF') +
  ggtitle("SHAP values of YearBuilt")
g1 = g1 +scale_color_gradient(low="darkorchid", high="#7CAE00", guide=FALSE)  + labs(color = "Remodeling") +geom_point(size=0.8)
g1$layers[[2]] = NULL
g1

#OverallCond
shap_long <- shap.prep(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
g1 <- shap.plot.dependence(data_long = shap_long, x = 'OverallQual', y = 'OverallQual') +
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