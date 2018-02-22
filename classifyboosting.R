rm(list = ls()) #clears workspace

source("files.R")
source("makeWebsiteString.R")
#install.packages("randomForest") # Vet inte om det ska finnas n?gra package installs h?r/var? Vet inte om install.packages("rpart") beh?vs ox?? /2Lunch
source("neuralNetwork.R")

 write_to_file = function(name, title, predicts){
   txt <- makeWebsiteString(predicts)
   txt = paste(title, txt)
   write(txt, file=name, append = TRUE)
}

songs.train <- read.csv('training_data.csv', header = T)
songs.classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = list("label", "key", "mode", "time_signature")
songs.train = setQualitative(songs.train, qualitative_vars)
songs.classify = setQualitative(songs.classify, qualitative_vars)

filename = "Boosting_results.txt"
 
txt = "Start of file"
write(txt, file = filename) #Empty results file

#BOOSTING
#installera adabag innan ni kör.
library(adabag)

#1. training
set.seed(666)
training_indices <- sample(nrow(songs.train), size = 375, replace = FALSE)

songs.training <- songs.train[training_indices,]
songs.testing <- songs.train[-training_indices,]

#Scale data between 0 and 1
songs.training = scale_data(songs.training, qualitative_vars)
songs.testing = scale_data(songs.testing, qualitative_vars)

# f = label ~ danceability + energy + key+ mode + loudness +speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration +  time_signature
# #Use AdaBoost ...
# boosting.fit <- boosting(formula=f,data=songs.training)
# #... and do prediction on test data
# boosting.pred <- predict(boosting.fit, newdata=songs.testing)
# #test error rate
# boosting.test.error <- mean(songs.testing$label != boosting.pred$class)
# boosting.test.error


#xdgboost
xDboost = function (train, test) {
  library(xgboost)
  
  # fit model
  bst <- xgboost(data = train[,1:28], label = train[,"like"], max.depth = 1, 
                 eta = 0.3, nround = 10, subsample = 0.5, colsample_by_tree=0.5, objective = "binary:logistic")
  # predict
  pred <- predict(bst, test)
  
  
  xgboosting.test.error <- mean(test[, "like"] != round(pred))
  return(xgboosting.test.error)
}
# songs.train = scale_data(songs.train, qualitative_vars)
# songs.classify = scale_data(songs.classify, qualitative_vars)
# 
# songs.train = data.matrix(classify(songs.train))
# keys = c("C", "C_major", "D_minor", "D", "D_major", "E_minor", "E", "E_major", "F_minor", "F", "F_major", "G_minor")
# 
# songs.classify = cbind(songs.classify[,1:10], class.ind(as.factor(songs.classify$key)), class.ind(as.factor(songs.classify$time_signature)), class.ind(as.factor(songs.classify$mode)),class.ind(as.factor(songs.classify$label)))
# names(songs.classify) <- c(names(songs.classify)[1:10], keys, "onefourths", "threefourths", "fourfourths", "minor", "major")
# 
# error = xDboost(songs.train, data.matrix(songs.classify))

#2. implementation
songs.train = scale_data(songs.train, qualitative_vars)
songs.classify = scale_data(songs.classify, qualitative_vars)
f = label ~ danceability + energy + key+ mode + loudness +speechiness + acousticness + instrumentalness + liveness + valence + tempo + duration +  time_signature 
boost.fit <- boosting(formula=f,data=songs.train)
boost.pred <- predict(boost.fit, newdata=songs.classify)
# 
write_to_file(filename,"B-b-b-boosting:", boost.pred$class)
# 
# txt = "End of file"
# write(txt, file = filename, append = TRUE)

