library(iml)
library(xgboost)

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

mod <- Predictor$new(xgmodel, data = training_data)

#-------------------
#YearBuilt

#ALE
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="ale", grid.size = 100)
eff$plot()

#PDP
eff <- FeatureEffect$new(mod, feature = "YearBuilt", method="pdp", grid.size = 100)
eff$plot()

#-------------------
#GrLivArea

#ALE
eff <- FeatureEffect$new(mod, feature = "GrLivArea", method="ale", grid.size = 500)
eff$plot() + xlim(0, 3500)

#PDP
eff <- FeatureEffect$new(mod, feature = "GrLivArea",  method="pdp", grid.size = 500)
eff$plot() + xlim(0, 3500)

#-------------------
#LotArea

#ALE
eff <- FeatureEffect$new(mod, feature = "LotArea", method="ale", grid.size = 100)
eff$plot()

#PDP
eff <- FeatureEffect$new(mod, feature = "LotArea", method="pdp", grid.size = 100)
eff$plot()

#--------------
#TotalBsmtSF

#PDP
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 60)
eff$plot() + xlim(0, 2000) + ylim(165000, 225000) + ylab("PDP")


#ALE
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 60)
eff$plot() + xlim(0, 2000) + ylim(-18000, 42000) + ylab("ALE")


#Combination
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="pdp", grid.size = 500)
eff.dat_pdp <- eff$results %>% mutate(.value = .value-min(.value))
eff <- FeatureEffect$new(mod, feature = "TotalBsmtSF", method="ale", grid.size = 500)
eff.dat_ale<- eff$results %>% mutate(.value = .value-min(.value))
ggplot() + 
  geom_line(aes(y = eff.dat_ale$.value, x=eff.dat_ale$TotalBsmtSF), color = "darkred") + 
  geom_line(aes(y = eff.dat_pdp$.value, x=eff.dat_pdp$TotalBsmtSF), color="steelblue", linetype="twodash") +
  xlim(0, 1800) + ylim(-1000, 50000) 

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

#ALE
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="ale")
eff$plot()

#PDP
eff <- FeatureEffect$new(mod, feature = "OverallQual", method="pdp")
eff$plot()

#------------------------
#YearBuilt + Remod
eff <- FeatureEffect$new(mod, feature = c("YearBuilt", "Remod"), grid.size = 30, method="ale")
eff$plot() + scale_color_grey()
plot(eff) +   #scale_fill_gradient(low="blue", high="red") + 
  ggtitle("ALE") + scale_fill_viridis_c() +
  geom_point(data = training_data, aes(y =Remod, x = YearBuilt),color = "black", size = 1)

training_data_remod_0 = readRDS(paste0(path, "/results/training_data.rds")) %>% filter(Remod == 0)
training_data_remod_1 = readRDS(paste0(path, "/results/training_data.rds")) %>% filter(Remod == 1)
mod_remod_0  <- Predictor$new(xgmodel, data = training_data_remod_0)
mod_remod_1  <- Predictor$new(xgmodel, data = training_data_remod_1)

eff_remod_0 <- FeatureEffect$new(mod_remod_0, feature = "YearBuilt", grid.size = 30, method="ale")
eff_remod_0$plot()
eff_remod_1  <- FeatureEffect$new(mod_remod_1, feature = "YearBuilt", grid.size = 30, method="ale")
eff_remod_1$plot()


eff.dat <- eff$results 
eff_remod_0 = eff.dat %>% filter(Remod==0) %>% select(-Remod)
eff_remod_1 = eff.dat %>% filter(Remod==1)%>% select(- Remod)

eff$results = eff_remod_0
eff$
eff$plot()
eff$results = eff_remod_1


eff_remod_0$plot()
