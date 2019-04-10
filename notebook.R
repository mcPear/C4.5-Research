# load the libraries
library(caret)
library(MLmetrics)
source("/home/maciej/myProjects/decision_tree/c45.R")

# load datasets
glass <- read.csv("/home/maciej/myProjects/decision_tree/data/glass_data", header = FALSE)
wine <- read.csv("/home/maciej/myProjects/decision_tree/data/wine_data", header = FALSE)
pima <- read.csv("/home/maciej/myProjects/decision_tree/data/pima_data", header = FALSE)
iris <- read.csv("/home/maciej/myProjects/decision_tree/data/iris_data", header = FALSE)


for(folds in c(2,3,5,10)){
  model <- c45(iris, folds, C = 0.25, M = 2, N = 3, Q = 1, R = FALSE, stratified = TRUE)
  # print(model$resample)
  print(model$results$f1)
}