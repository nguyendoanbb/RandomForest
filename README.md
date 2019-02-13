#Description 
For binary classification problem, 'trainforest' automatically computes cross validation for features selection, searches for optimal number of variables randomly sampled at each split, generates random forest model with features importance plot, and output metric based on classification or regression problem. 

This function utilizes built-in functions from various packages such as 'randomForest', 'caret', 'ggplot2'. User needs to split dataset into train and test set before input data into trainforest.

#Description:
  -response: a train data frame/vector of dependent variable
  -features: a train data frame/vector of selected features
  -new.response: a test data frame/vector of dependent variable
  -new.features: a test data frame/vector of selected features
  -createPlot: create a plot of features importance
  -cv.fold: number of k-fold cross validation
  -cf.step: the fraction of varibles to remove at each step in cross validation
  -cv.recursive: whether variable iportanceimportance is (re)assessed at each step of variable reduction in cross validation
  -tune.improve: the improveent in OOB error must be by this much for the search to continue
  -tune.trace: whether to print the progress of the search in tuning mtry
  
  
#Example

```R
library(faraway)
data('pima')
str(pima)
for (i in c(1:length(pima))){
  if(is.integer(pima[,i]) == TRUE){
    pima[,i] <- as.numeric(pima[,i])
  }
}

train <- sample(1:nrow(pima), nrow(pima)*2/3)
xx <- pima[train, -length(pima)]
yy <- pima[train, length(pima)]
test.xx <- pima[-train, -length(pima)]
test.yy <- pima[-train, length(pima)]

trainforest(response = as.factor(yy),
            features = xx,
            new.response = as.factor(test.yy),
            new.features = test.xx,
            cv.fold = 10,
            cv.step = 0.8)
```