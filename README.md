# Interpretable Machine Learning Seminar
Project code for the Interpretable Machine Learning (IML) Seminar using the Ames Housing Prices dataset.

## Dataset & Overview
The dataset for this project can be found [here](data/train.csv) in our repository, or be downloaded directly from [Kaggle](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data).

If you want to recreate the analysis set the working directoy to the project folder. Then you can run the following files in this order:

| File                                                                               | Content                                                  |
|------------------------------------------------------------------------------------|----------------------------------------------------------|
| [cleaning_train.R](cleaning_train.R)                               | Data cleaning and pre-processing. Called by [simple_models.R](simple_models.R) and [xgboost_train.R](xgboost_train.R) to get the ready-to-use dataset (see section 1 in the report).
| [simple_models.R](simple_models.R)                                     | Implementation and performance results of our 3 baseline (interpretable) models (see section 2 in the report).                     |
| [xgboost_train.R](xgboost_train.R)                                                 | Implementation and performance results of our black box model - XGBoost. Saves the model for future use for our IML methods (see section 2 in the report).          |
| [outlier_detection.R](outlier_detection.R)                                                 | Analyze the dataset for presence of outliers (see section 3 in the report).           |
| [outlier_analysis.R](outlier_analysis.R)                                                 | More in-depth look at the detected outliers (see section 3 in the report).            |
| [feature_importance.R](feature_importance.R) | Implementation of feature importance algorithms for XGBoost and Linear Regression (see section 4.1 in the report).                               |
| [specific_hypotheses.R](specific_hypotheses.R) | Analysis of hypotheses related to specific features (see section 4.2 in the report)                              |
| [specific_points.R](specific_points.R) | Analysis of hypotheses related to specific points (see section 4.3 in the report)                              |

Files also contain extensive commentary for better code flow comprehension.

## Software Requirements
The following module versions (or above) are required to reproduce the code:

R = 4.0.3  

### Data Cleaning and Preprocessing
dplyr = 1.0.2  
plyr = 1.8.6  
tidyverse = 1.3.0  
readr = 1.4.0     
ggplot2 = 3.3.2  
corrplot = 0.84  
caret = 6.0.86  
Hmisc = 4.4.2  
Metrics = 0.1.4  
viridis = 0.5.1  
partykit = 1.2.11  
variables = 1.0.3  
forcats = 0.5.0  
data.table = 1.14.0


### Models
xgboost = 1.2.0.1  
rpart = 4.1.15  
rpart.plot = 3.0.9  
glmnet = 4.0.2  
cmaes = 1.0.11  
mlr = 2.18.0  
mlrCPO = 0.3.7

### Interpretable ML Methods
iml = 0.10.1  
SHAPforxgboost = 0.1.0  
featureImportance = 0.9  

### Counterfactuals
Require a manual installation from github!
The github repository can be found [here](https://github.com/susanne-207/moc). The necessary packages are *counterfactuals* and the corresponding version of the *iml* package, which is different from the one listed above. We manually call the respective packages in our code (see [specific_points.R](specific_points.R)). 
