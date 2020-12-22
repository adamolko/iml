library(ggplot2)
library(tidyverse)

ada_check = TRUE

if(ada_check){
  path = ""
  path_train <- "data/train.csv"
  source("cleaning_train.R")
} else{
  path ="C:/R - Workspace/IML/"
}

xgmodel <-  readRDS(paste0(path, "results/xgboost_model.rds"))
training_data <-  readRDS(paste0(path, "results/training_data.rds"))
shap_values <-  readRDS(paste0(path, "results/shap_values.rds"))
shap_long <- readRDS(paste0(path, "results/shap_long.rds"))

bias = shap_values$BIAS0

outliers <- training_data %>% filter(GrLivArea >= 3551)
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


p1<-ggplot(data=point1_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 1"),
       subtitle = paste0("Predicted Sale Price: ", round(point1_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-25000,
            size=3)+ xlim(-30000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p1


p2<-ggplot(data=point2_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 2"),
       subtitle = paste0("Predicted Sale Price: ", round(point2_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-45000,
            size=3)+ xlim(-50000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p2

p3<-ggplot(data=point3_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 3"),
       subtitle = paste0("Predicted Sale Price: ", round(point3_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-25000,
            size=3)+ xlim(-30000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p3

p4<-ggplot(data=point4_shap_plot, aes(x=value, y= reorder(paste0(variable, ": ", rfvalue), abs(value)))) +
  labs(title = paste0("Shapley values for observation 4"),
       subtitle = paste0("Predicted Sale Price: ", round(point4_shap_sum,0))) +
  geom_col( aes(fill = abs(value))) +xlab("SHAP value") + ylab("Variable") +
  geom_text(aes(label=round(value, digits = 0)), color="black", x=-55000,
            size=3)+ xlim(-60000, NA) +
  #hjust=-1*sign(value)
  scale_fill_gradient(name="SHAP value (abs.)")
p4

g1 <- ggplot(training_data, aes(x=GrLivArea, y=SalePrice, colour = GrLivArea < 3551)) +
  geom_point() + scale_x_continuous(limits = c(0,6000))+
  scale_y_continuous(limits = c(0,800000))+
  scale_color_discrete(name="Living Area > 3551", labels=c("Yes", "No")) + 
  geom_smooth(method = lm, color = "black", fullrange = TRUE)
g1

#create dataframe without two possible outliers
row.names.remove <- c("1299", "524")
without_outliers <- training_data[!(row.names(training_data) %in% row.names.remove), ]

g2 <- ggplot(without_outliers, aes(x=GrLivArea, y=SalePrice, colour = GrLivArea < 3551)) +
  geom_point() + scale_x_continuous(limits = c(0,6000))+
  scale_y_continuous(limits = c(0,800000))+
  scale_color_discrete(name="Living Area > 3551", labels=c("Yes", "No")) + 
  geom_smooth(method = lm, color = "black", fullrange = TRUE)
g2
#used geom_smooth() to check out how the trend changes if you remove the potential outliers
#apparently they are not that influencial

features <- c("SalePrice", "OverallQual", "GrLivArea", "porch_area", "LotArea", "GarageCars",
              "TotalBsmtSF", "ExterQual", "FullBath", "OverallCond", "KitchenQual", 
              "BedroomAbvGr", "numb_add_flr", "YearBuilt")

outlier_features <- select(outliers, features)
#check the features that are the top ones for shapley values

library(data.table) 
# transpose the dataframe to easier compare the feature values
t_outliers <- transpose(outlier_features)

# get row and colnames in order
colnames(t_outliers) <- rownames(outlier_features)
rownames(t_outliers) <- colnames(outlier_features)
t_outliers

#plotting the table for comparison of two specific points
compare_table <- t_outliers[, c(3,4)]
colnames(compare_table) <- c("point 3", "point 4")
library(gridExtra)
ggplot()
grid.table(compare_table)

#since point 3 seems to follow the general trend, we compare it to point 4, which has a 
#surprisingly low SalePrice

#shapley value for porch_area is negative for point 4, which makes sense, the less the 
#porch area, the lower the price

#the lot area is almost twice as large for point 4 compared to point 3
#that explains why the shapley value for LotArea is negative for point 4, because it is simply too much

#another notably differing feature value is that of OverallCond
#for point 4 it is only 5 out 10, and since for both point 3 and point 4 OverallCond 
#is quite important according to shapley values, it makes sense that for point 4 it is negative

#other features seem to have more or less same values

#so basically point 4 is not that much of an outlier, because largely differing values
#can be explained by the shapley values
