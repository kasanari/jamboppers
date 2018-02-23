library(randomForest)
set.seed(1)

songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 400, replace = FALSE)

qualitative_vars = list("label","key", "time_signature", "mode")
songs = setQualitative(songs, qualitative_vars)

songs.train = songs[training_indices,]
songs.test = songs[-training_indices,]

serendipityGrove <- function(train, test, f) {
  B <- 100
  
  rf.fit <- randomForest(f, data = train, ntree = B)
  rf.pred <- predict(rf.fit, newdata = test)
  
  return(rf.pred)
}


treeAnalysis = function(train, test, f) {
  error = list();
 for (i in 1:500) {
   B <- i
   rf.fit <- randomForest(f, data = train, ntree = B, importance = TRUE)
   rf.pred <- predict(rf.fit, newdata = test)
   error[i] = mean(rf.pred == test$label)
 }
  importance(rf.fit)
  varImpPlot(rf.fit)
  return(which.min(error))
}