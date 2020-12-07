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

#get the model & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
lm = readRDS(paste0(path, "/results/linear_regression.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))


#--------------------
#Describe the two points from SHAP dependence plot:

#These are the two points: 
point1 = training_data %>% filter( GrLivArea ==1698  & OverallQual > 8) #ID is 725
point2 = training_data %>% filter( GrLivArea ==1699  & OverallQual < 5) #ID is 1187
points = bind_rows(point1, point2)
#Describe them in presentation (maybe complete description?)
points %>% select(OverallQual)
points %>% select(GrLivArea)

#Need to calculate shap values for them (ID changes for some reason then...)
shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
bias = shap_values$BIAS0
shap_long <- shap.prep(shap_contrib = shap_values$shap_score,  X_train = select(training_data, - SalePrice))
point1_finder = shap_long %>% filter(rfvalue == 1698 , variable == "GrLivArea") %>% pull(ID)
point2_finder = shap_long %>% filter(rfvalue == 1699 , variable == "GrLivArea") %>% pull(ID)

point1_shap = shap_long %>% filter(ID == point1_finder) %>% arrange(desc(abs(value)))
point1_shap_sum = sum(point1_shap %>% pull(value)) + bias
point2_shap = shap_long %>% filter(ID == point2_finder) %>% arrange(desc(abs(value)))
point2_shap_sum = sum(point2_shap %>% pull(value)) + bias
point1_shap_plot = point1_shap %>% slice(1:15)
point2_shap_plot = point2_shap %>% slice(1:15)

p1<-ggplot(data=point1_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation with high OverallQual"),
         subtitle = paste0("Predicted Sale Price: ", round(point1_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-25000,
           size=3)+ xlim(-30000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p1
ggsave(filename = paste0(path, "/results/SHAP_values_observation_high_quality.jpg"), plot = p1)

p2<-ggplot(data=point2_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation with low OverallQual"),
       subtitle = paste0("Predicted Sale Price: ", round(point2_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-30000,
            size=3)+ xlim(-35000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p2
ggsave(filename = paste0(path, "/results/SHAP_values_observation_low_quality.jpg"), plot = p2)


#--------------------------------
#Lets pick observation 2 and du further analysis

#---------
#LocalModel
mod <- Predictor$new(xgmodel, data = training_data)
# Explain the first instance of the dataset with the LocalModel method:
point2
#We need some explanation/reasoning/idea on how many features (k) to choose

#Choose 10 for now
local_model <- LocalModel$new(mod, x.interest = point2, k = 10)
# Look at the results in a table
local_model$results
# Or as a plot
g1 = plot(local_model)
g1
ggsave(filename = paste0(path, "/results/Local_Model_prediction_k10.jpg"), plot = g1)
#Choose 30 
local_model <- LocalModel$new(mod, x.interest = point2, k = 30)
local_model$results
g2 = plot(local_model)
g2
ggsave(filename = paste0(path, "/results/Local_Model_prediction_k30.jpg"), plot = g2)

#Choose 50 
local_model <- LocalModel$new(mod, x.interest = point2, k = 50)
local_model$results
g3 = plot(local_model)
g3
ggsave(filename = paste0(path, "/results/Local_Model_prediction_k50.jpg"), plot = g3)

#My takeaway:
#Fidelity is low, if k chosen between 2 and 20 
# ---> can't really use it for our question on how to change features to increase prediction
# ---> but interpretation a lot easier, how prediction is made
#Fidelity better, if we further increase k
# ---> then we can use it, but: how good  is generalization then?
# ---> interpretation of prediction also much worse


#---------
#Counterfactuals
devtools::load_all("C:/R - Workspace/moc/counterfactuals", export_all = FALSE)
devtools::load_all("C:/R - Workspace/moc/iml", export_all = FALSE)
library("mlr")
library("mlrCPO")
library("ggplot2")
library(tidyverse)
library(partykit)
#library(trft)
library(variables)
best.params = readRDS("C:/R - Workspace/moc/saved_objects/best_configs.rds")
set.seed(1234)
pred = Predictor$new(xgmodel, data = training_data)
# ctr = partykit::ctree_control(maxdepth = 5L)
# pred$conditionals = fit_conditionals(pred$data$get.x(), ctrl = ctr)
#point2 = training_data %>% filter( GrLivArea ==1699  & OverallQual < 5) #ID is 1187
#pred$predict(point2)
not_categories <- c("LotArea", "YearBuilt", "TotalBsmtSF",
                 "GrLivArea", "porch_area", "SalePrice")
variables = colnames(training_data)
categories <- variables[which(!variables %in% not_categories)]
for(category in categories){
  print(category)
  pred[["data"]][["X"]][[category]] = as.integer( pred[["data"]][["X"]][[category]])
  #pred[["data"]][["X"]][[category]] = as.factor(pred[["data"]][["X"]][[category]])
  #point2[[category]] = as.integer(point2[[category]])
  #point2[[category]] = as.factor(point2[[category]])
  
}
point2_other =  pred[["data"]][["X"]] %>% filter( GrLivArea ==1699  & OverallQual == 3) %>% add_column(SalePrice = 95000, .after = "porch_area")
point2_other = as.data.frame(point2_other)



#Set dummies of categories to not change:
# --> fixed.features
list_features_not_changing = c("MSZoning.RL" ,"MSZoning.RM" ,"MSZoning.C..all.", "MSZoning.FV", "LotShape.Reg",
                               "LotShape.IR1", "Neighborhood.CollgCr", "Neighborhood.Veenker", "Neighborhood.Crawfor",
                               "Neighborhood.NoRidge", "Neighborhood.Mitchel", "Neighborhood.Somerst", "Neighborhood.NWAmes",
                               "Neighborhood.OldTown",  "Neighborhood.BrkSide", "Neighborhood.Sawyer",
                               "Neighborhood.NridgHt", "Neighborhood.NAmes", "Neighborhood.SawyerW", "Neighborhood.IDOTRR", 
                               "Neighborhood.MeadowV", "Neighborhood.Edwards", "Neighborhood.Timber",
                               "Neighborhood.Gilbert", "Neighborhood.StoneBr", "Neighborhood.ClearCr",
                               "Neighborhood.NPkVill", "Neighborhood.Blmngtn", "Neighborhood.BrDale", "Neighborhood.SWISU",
                               "BldgType.1Fam", "BldgType.2fmCon", "BldgType.Duplex", "BldgType.TwnhsE",
                               "RoofStyle.Gable", "RoofStyle.Hip", "Foundation.PConc", "Foundation.CBlock", "Foundation.BrkTil",
                               "Functional.Typ", "Functional.Min", "SaleType.WD", "SaleType.New", "SaleType.COD",
                               "SaleCondition.Normal", "SaleCondition.Abnorml","SaleCondition.Partial", "SaleCondition.Family",
                               "SeasonSold.sp", "SeasonSold.su", "SeasonSold.a", "porch_type.enclosed_porch", "porch_type.multiple",
                               "porch_type.open_porch", "porch_type.screen_porch", "porch_type.three_s_porch", "porch_type.wood_deck")
                              

counterfactual = Counterfactuals$new(pred, x.interest = point2_other,
                                     target = 110000, generations = 20, track.infeas=TRUE, epsilon = 200,
                                     fixed.features = list_features_not_changing)
# system.time({credit.cf = Counterfactuals$new(predictor = pred, 
#                                              x.interest = point2_other, 
#                                              target = 120000, epsilon = 200, generations = list(mosmafs::mosmafsTermStagnationHV(10),
#                                                                                                  mosmafs::mosmafsTermGenerations(200)), 
#                                              mu = best.params$mu, 
#                                              p.mut = best.params$p.mut, p.rec = best.params$p.rec, 
#                                              p.mut.gen = best.params$p.mut.gen, 
#                                              p.mut.use.orig = best.params$p.mut.use.orig, 
#                                              p.rec.gen = best.params$p.rec.gen, initialization = "icecurve",
#                                              p.rec.use.orig = best.params$p.rec.use.orig)})


actual_counterfactuals = counterfactual$results$counterfactuals
diff_counterfactuals = counterfactual$results$counterfactuals.diff

counterfactuals_results = counterfactual$results
counterfactuals_results
saveRDS(file = paste0(path, "/results/counterfactuals_results.rds"), object = counterfactuals_results)


