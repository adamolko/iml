library(xgboost)
library(tidyverse)
library(mlr)
library(rpart)
library(rpart.plot)

#get the model, tuning parameters & the training data first
xgmodel = readRDS(paste0(path, "results/xgboost_model.rds"))
training_data = readRDS(paste0(path, "results/training_data.rds"))
tuning_result <- readRDS(paste0(path, "results/tuning_result.rds"))
parameters = tuning_result$x 
lrn_tune <- setHyperPars(lrn,par.vals = parameters)  



#define calculation of influence:
influence.v = function(predicted, predicted.without) {
  predicted - predicted.without
}

set.seed(123)
training_data = training_data %>% arrange(as.numeric(rownames(training_data)))
traintask <- makeRegrTask(data = training_data,target = "SalePrice")
xgpred <- predict(xgmodel,traintask)
predicted.orig = xgpred$data$response

cs = lapply(1:nrow(training_data), function(to.remove.index) {
  training_temp = training_data[-to.remove.index,]
  traintask_temp <- makeRegrTask(data = training_temp,target = "SalePrice")
  xgboost_temp = train(learner = lrn_tune, task = traintask_temp)
  
  xgpred_temp <- predict(xgboost_temp,traintask)
  predict.removed =  xgpred_temp$data$response
  influence.v(predicted.orig, predict.removed)
})

influence.df = data.frame(cs)
influence.df = as.matrix(influence.df)
diag(influence.df) = NA
saveRDS(influence.df, paste0(path, "results/influence.df.rds"))

influence.df = readRDS(paste0(path, "results/influence.df.rds"))
influence_avg = data.frame(influence = colMeans(abs(influence.df), na.rm = TRUE), id = 1:nrow(training_data))
influence_avg = influence_avg[order(influence_avg$influence, decreasing = TRUE),]

#Compare Influence to mean and sd:
mean(abs(predicted.orig))
sd(predicted.orig)

training_data_influence = training_data
training_data_influence$InfluenceAvg = influence_avg %>% arrange(id) %>% select(influence)  %>% mutate(influence = as.numeric(influence)) %>% unlist()


regtree_model <- rpart(formula = InfluenceAvg ~ ., data = select(training_data_influence, -SalePrice), method = "anova",
                       control=rpart.control(minsplit=1, minbucket=1, cp=0.001,maxdepth = 3))
printcp(regtree_model)
rpart.plot(regtree_model, extra=101)


g = ggplot(training_data, aes(x=GrLivArea, y=SalePrice, colour = GrLivArea < 3551)) +
  geom_point() + 
  scale_color_discrete(name="Living Area > 3551", labels=c("Yes", "No"))
g
ggsave(filename = paste0(path, "results/scatter_for_influence.jpg"), plot = g, dpi=450)

ggplot(training_data, aes(x=GrLivArea, y=LotArea, colour = GrLivArea >= 623 & LotArea <2200 & porch_area < 543)) +
  geom_point()

# filter the dataframe for the observations in the right branch of the influence tree
training_data %>% filter(GrLivArea >= 3551)

shap_values <- shap.values(xgb_model = xgmodel$learner.model, X_train = select(training_data, - SalePrice))
bias = shap_values$BIAS0
shap_long <- shap.prep(shap_contrib = shap_values$shap_score,  X_train = select(training_data, - SalePrice))
point_finder = shap_long %>% filter(rfvalue >= 3551 , variable == "GrLivArea")  %>% pull(ID)

point1_shap = shap_long %>% filter(ID == point_finder[1]) %>% arrange(desc(abs(value)))
point1_shap_sum = sum(point1_shap %>% pull(value)) + bias
point2_shap = shap_long %>% filter(ID == point_finder[2]) %>% arrange(desc(abs(value)))
point2_shap_sum = sum(point2_shap %>% pull(value)) + bias
point3_shap = shap_long %>% filter(ID == point_finder[3]) %>% arrange(desc(abs(value)))
point3_shap_sum = sum(point3_shap %>% pull(value)) + bias
point4_shap = shap_long %>% filter(ID == point_finder[4]) %>% arrange(desc(abs(value)))
point4_shap_sum = sum(point4_shap %>% pull(value)) + bias


point1_shap_plot = point1_shap %>% slice(1:15)
point2_shap_plot = point2_shap %>% slice(1:15)
point3_shap_plot = point3_shap %>% slice(1:15)
point4_shap_plot = point4_shap %>% slice(1:15)

# plotting the shapley values
p1<-ggplot(data=point1_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 1"),
       subtitle = paste0("Predicted Sale Price: ", round(point1_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-25000,
            size=3)+ xlim(-30000, NA) +
  scale_fill_gradient(name="SHAP value (abs.)")
p1


p2<-ggplot(data=point2_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 2"),
       subtitle = paste0("Predicted Sale Price: ", round(point2_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-45000,
            size=3)+ xlim(-50000, NA) +
  scale_fill_gradient(name="SHAP value (abs.)")
p2

p3<-ggplot(data=point3_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 3"),
       subtitle = paste0("Predicted Sale Price: ", round(point3_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-25000,
            size=3)+ xlim(-30000, NA) +
  scale_fill_gradient(name="SHAP value (abs.)")
p3

p4<-ggplot(data=point4_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 4"),
       subtitle = paste0("Predicted Sale Price: ", round(point4_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-55000,
            size=3)+ xlim(-60000, NA) +
  scale_fill_gradient(name="SHAP value (abs.)")
p4

