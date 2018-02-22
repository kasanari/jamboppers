#rm(list = ls()) #clears workspace
#set.seed(999)
source("files.R")
library(neuralnet)
library(nnet)


neural_network = function(train, test, s_weights) {
  
  ###Neural Network###
  n <- names(train)
  learning_vars = n[!n %in% c("like")]
  string_learning = paste(learning_vars, collapse = " + ")
  formel = paste("like ~", string_learning)
  f <- as.formula(formel)
  
  good = c(15, 10, 10, 2)
  
  nn <- neuralnet(f, data=train, hidden=good, threshold = 0.1, rep=50, linear.output=FALSE, startweights = s_weights)
  
  #plot(nn)
  
  ##Predictions###
  
  t  = test[, learning_vars]
  
  pr.nn <- compute(nn, t)
  
  answer = round(pr.nn[["net.result"]])
  
  answer.verbose <- rep("dislike", length(answer))
  answer.verbose[answer == 1] <- "like"
  
  return(list(answer, nn$weights))
}

scale_data = function(data, qualitative_vars) {
  
  data_to_scale = data[, !(names(data) %in% qualitative_vars)]
  maxs <- apply(data_to_scale, 2, max) 
  mins <- apply(data_to_scale, 2, min)
  
  scaled <- as.data.frame(scale(data_to_scale, center = mins, scale = maxs - mins))
  
  for (var in qualitative_vars) {
    scaled[[var]] = data[[var]]
  }
  return(scaled)
}

classify = function(data) {
  # Encode as a one hot vector multilabel data
  songs.class <- cbind(data[1:10], class.ind(as.factor(data$key)), class.ind(as.factor(data$time_signature)), class.ind(as.factor(data$mode)),class.ind(as.factor(data$label)))
  keys = c("C", "C_major", "D_minor", "D", "D_major", "E_minor", "E", "E_major", "F_minor", "F", "F_major", "G_minor")
  # Set labels name
  songs.class = cbind(songs.class[,1:28], songs.class[,30])
  names(songs.class) <- c(names(data)[1:10], keys, "onefourths", "threefourths", "fourfourths", "fivefourths", "minor", "major", "like")
  return(songs.class)
}

save_old_nn = function(old_nn) {
  save(old_nn, file = "nn.data")
}


# songs <- read.csv('training_data.csv', header = T)
# 
# training_indices <- sample(nrow(songs), size = 400, replace = FALSE)
# 
# qualitative_vars = c("key", "time_signature", "mode", "label")
# songs = setQualitative(songs, qualitative_vars)
# 
# scaled = scale_data(songs, qualitative_vars)

#songs.class = classify(scaled)

#train <- songs.class[training_indices,]
#test <- songs.class[-training_indices,]


#answer = neural_network(train, test)

#test.error <- mean(test[c("like", "dislike")] != answer[[1]])

