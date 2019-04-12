library(caret)
library(MLmetrics)

c45 <- function(data, folds, C, M, N, Q, R){ # if R then N else C
  data <- data[sample(nrow(data)),]

  x_data <- data[,1:ncol(data)-1]
  y_data <- as.factor(data[,ncol(data)])
  
  custom_metric <- function (data, lev = NULL, model = NULL) {
    accuracy <- Accuracy(data$obs, data$pred)
    precision <- Precision(data$obs, data$pred)
    recall <- Recall(data$obs, data$pred)
    f1 <- F1_Score(data$obs, data$pred)
    
    names(accuracy) <- c("accuracy")
    names(precision) <- c("precision")
    names(recall) <- c("recall")
    names(f1) <- c("f1")
    
    c(accuracy, precision, recall, f1)
  } 
  
  strIndex <- createFolds(y_data, k = folds, returnTrain = T)
  
  tc <- trainControl(
    index = strIndex,
    method = 'cv',
    number = folds,
    summaryFunction = custom_metric)
  
  c45 = getModelInfo("J48", FALSE)[[1]]
  
  c45$parameters <- data.frame("parameter" = c("C","M","N","Q","R"), 
                               "class" = c("numeric","numeric","numeric","numeric","boolean"), 
                               "label" = c("Confidence Threshold","Minimum Instances Per Leaf","Number of folds","Seed","Use reduced error pruning"))
  
  c45$grid <- function (x, y, len = NULL, search = "grid") 
  {
    return(data.frame(C = C, M = M, N = N, Q = Q, R = R))
  }
  
  c45$fit <- function (x, y, wts, param, lev, last, classProbs, ...) 
  {
    
    dat <- if (is.data.frame(x)) 
      x
    else as.data.frame(x)
    dat$.outcome <- y
    theDots <- list(...)
    if (any(names(theDots) == "control")) {
      theDots$control$C <- param$C
      theDots$control$M <- param$M
      theDots$control$N <- param$N
      theDots$control$Q <- param$Q
      ctl <- theDots$control
      theDots$control <- NULL
    }
    else {
      if (param$R) ctl <- RWeka::Weka_control(M = param$M, Q = param$Q, N = param$N, R = param$R)
      else ctl <- RWeka::Weka_control(C = param$C, M = param$M, Q = param$Q)
    }
    modelArgs <- c(list(formula = as.formula(".outcome ~ ."), 
                        data = dat, control = ctl), theDots)

    out <- do.call(RWeka::J48, modelArgs)
    out
  }
  
  model <- train(x = x_data, y = y_data, trControl=tc, method=c45, metric = "accuracy")
  return(model)
}