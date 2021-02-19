# Interpretable Machine Learning Seminar
Project for Interpretable Machine Learning using Housing Prices dataset.

## Dataset & Overview
The benchmark dataset for this project can be found [here](data/ml-100k/u.data) in our repository, or be downloaded directly from the [Website](https://grouplens.org/datasets/movielens/100k/).

If you want to recreate the analysis you can run the following files in this order:

| File                                                                               | Content                                                  |
|------------------------------------------------------------------------------------|----------------------------------------------------------|
| [movielens_descriptives.R](movielens_descriptives.R)                               | Analyze and describe dataset, plot feature distributions |
| [model_selection.ipynb](model_selection.ipynb)                                     | Hyperparametertuning for kNN and SVD                     |
| [scenario1.ipynb](scenario1.ipynb)                                                 | Analyze change in prediction for review bombing          |
| [scenario2.ipynb](scenario2.ipynb)                                                 | Analyze change in prediction for paid reviews            |
| [Fake_Ratings_in_Recommender_Systems.pdf](Fake_Ratings_in_Recommender_Systems.pdf) | In-depth report of analysis                              |

Descriptions of the code are given in the respective file.

## Software Requirements
In order to run the R code, you will need the following
module versions:

R = 4.0.3  
gridExtra = 2.3   
dplyr = 1.0.2  
readr = 1.4.0     
ggplot2 = 3.3.3  
