library(randomForest)
set.seed(1)

serendipityGrove <- function(train, test, f) {
  
  B <- 100
  
  rf.fit <- randomForest(f, data = train, ntree = B)
  rf.pred <- predict(rf.fit, newdata = test, predict.all = TRUE) # predict.all = TRUE - store predictions for all trees
  
  return(rf.pred)
}