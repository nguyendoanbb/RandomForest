trainforest <- function(response,features, new.response, new.features,
                        createPlot = TRUE, cv.fold, cv.step, cv.recursive = F,
                        tune.improve = 0.01, tune.trace = T){
  library(randomForest)
  library(caret)
  library(dplyr)
  #Implement 10-fold CV for features selection
  
  cv <- rfcv(trainx = features, 
             trainy = response,
             cv.fold = cv.fold,
             step = cv.step,
             recursive = cv.recursive)
  cv.error <- as.table(cv$error.cv)
  min.feature <- cv.error[cv.error == min(cv.error)]
  min.feature <- as.numeric(attributes(min.feature)$names)
  bag <- randomForest(x=features, y=response, importance = TRUE)
  var.imp <- varImp(bag)
  
  #classification
  if (is.factor(response) == TRUE){
    var.imp$Var <- c(row.names(var.imp))
    var.imp <- var.imp[,-1]
    colnames(var.imp) <- c('Overall', 'Var')
    row.names(var.imp) <- NULL
    var.imp <- var.imp[order(var.imp$Overall, decreasing = TRUE),]
    min.var <- as.vector(var.imp$Var)[1:min.feature] #min numbers of features for lowest error
  }
  #regression
  else{
    var.imp$Var <- c(row.names(var.imp))
    var.imp <- var.imp[order(var.imp$Overall, decreasing = TRUE),]
    row.names(var.imp) <- NULL
    min.var <- as.vector(var.imp$Var)[1:min.feature] #min numbers of features for lowest error
  }
  #tuning for optimal mtry at each node
  optimal <- tuneRF(x = features[,c(min.var)], y = response, ntreeTry = 500, stepFactor = 5,
                    improve = tune.improve, doBest = TRUE, plot = F, trace = tune.trace)
  #prediction using optimal model
  pred <- predict(optimal, newdata = new.features)
  
  #classification
  if (is.factor(response) == TRUE){
    metric <- confusionMatrix(pred, new.response)
    cat('Accuracy is', metric$overall)
    cat('\n')
    cat('Minimum number of features needed:', min.var, sep = ' ')
  }
  
  #regression
  else{
    mse <- mean((pred - new.response)^2)
    cat('The MSE =', mse, sep = ' ')
    cat('\n')
    cat('Minimum number of features needed:', min.var, sep = ' ')
  }
  
  #plotting importance of features
  if (createPlot == TRUE){
    importance <- as.data.frame(importance(optimal))
    if (is.factor(response) == TRUE){
      varImportance <- data.frame(Variables = row.names(importance),
                                Importance= round(importance[,'MeanDecreaseGini'], 2))
    }
    else{
      varImportance <- data.frame(Variables = row.names(importance),
                                Importance= round(importance[,'IncNodePurity'], 2))
    }
    rankImportance <- varImportance %>%
      mutate(Rank = paste0('#',dense_rank(desc(Importance))))
    ggplot(rankImportance, aes(x = reorder(Variables, Importance) , 
                             y = Importance, fill = Importance)) +
      geom_bar(stat='identity') + 
      geom_text(aes(x = Variables, y = 0.5, label = Rank),
              hjust=0, vjust=0.55,size = 4, colour = 'red') +
      labs(x = 'Variables') +
      coord_flip() +
      theme_bw()
  }
}
