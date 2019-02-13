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
