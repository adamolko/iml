library(xgboost)
library(tidyverse)
library(mlr)
library(rpart)
library(rpart.plot)
ada_check = FALSE

if(ada_check){
  path = ""
  source(cleaning_path_train)
} else{
  path ="C:/R - Workspace/IML"
}

#get the model, tuning parameters & the training data first
xgmodel = readRDS(paste0(path, "/results/xgboost_model.rds"))
training_data = readRDS(paste0(path, "/results/training_data.rds"))
tuning_result <- readRDS(paste0(path, "/results/tuning_result.rds"))
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
saveRDS(influence.df, paste0(path, "/results/influence.df.rds"))

influence.df = readRDS(paste0(path, "/results/influence.df.rds"))
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
ggsave(filename = paste0(path, "/results/scatter_for_influence.jpg"), plot = g, dpi=450)

ggplot(training_data, aes(x=GrLivArea, y=LotArea, colour = GrLivArea >= 623 & LotArea <2200 & porch_area < 543)) +
  geom_point()

#TODO:
#look at those observations in more detail (features!)

training_data %>% filter(GrLivArea >= 3551)