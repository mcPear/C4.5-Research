# load the libraries
library(caret)
library(MLmetrics)
source("/home/maciej/myProjects/decision_tree/c45.R")

# load datasets
glass <- read.csv("/home/maciej/myProjects/decision_tree/data/glass_data", header = FALSE)
wine <- read.csv("/home/maciej/myProjects/decision_tree/data/wine_data", header = FALSE)
pima <- read.csv("/home/maciej/myProjects/decision_tree/data/pima_data", header = FALSE)
iris <- read.csv("/home/maciej/myProjects/decision_tree/data/iris_data", header = FALSE)
# if R then N else C

  acc <- c()
  rec <- c()
  prec <- c()
  f1 <- c()
  models <- c()
  # ms = c(1,2,5,10,25,50,100, 250)
  # ms = c(250)
  # cs = c(0.01, 0.1, 0.25, 0.5)
  # cs = c(0.5)
  # ns = c(2, 3, 4, 5, 6, 7, 8, 9, 10)
  ns = c(10)
  for (i in 1:length(ns)) {
    n = ns[i]
    model <- c45(pima, 10, C = 0.25, M = 50, N = n, Q = 1 , R = FALSE)
    acc = c(acc, model$results$accuracy)
    rec = c(rec, model$results$recall)
    prec = c(prec, model$results$precision)
    f1 = c(f1, model$results$f1)
    models = c(models, c(model))
  }

#   df = data.frame(
#     c(result$glass),
#     c(result$wine),
#     c(result$pima)
#   )
#   colnames(df) <- c("Glass", "Wine", "Pima")
#   rownames(df) <- c(" ", "  ", "   ", "     ")
#   
# print(df)
  df = data.frame(ns, acc, rec, prec, f1)
# print(result)
View(df)
plot(model$finalModel)
# plot(model$finalModel)
# text(model$finalModel)

# for(resample in model[["control"]][["indexOut"]]){
#   c1 <- resample <= 50
#   c3 <- resample >= 99
#   print(length(which(c1)))
#   print(length(which(c3)))
# }