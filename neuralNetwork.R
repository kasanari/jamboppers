#rm(list = ls()) #clears workspace
set.seed(666)
source("files.R")
library(neuralnet)
library(nnet)
load("nn.data")

neural_network = function(train, test) {
  
  ###Neural Network###
  n <- names(train)
  learning_vars = n[!n %in% c("like", "dislike")]
  string_learning = paste(learning_vars, collapse = " + ")
  formel = paste("like + dislike ~", string_learning)
  f <- as.formula(formel)
  
  good = c(15, 10, 10, 2)
  
  nn <- neuralnet(f, data=train, hidden=good, stepmax = 1e+08, threshold = 0.1, rep=5, linear.output=FALSE, startweights = old_nn$weights)
  
  #plot(nn)
  
  ##Predictions###
  
  t  = test[, learning_vars]
  
  pr.nn <- compute(nn, t)
  
  answer = round(pr.nn[["net.result"]])
  
  answer.verbose <- rep("dislike", length(answer))
  answer.verbose[answer == 1] <- "like"
  
  return(list(answer, nn))
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


songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 400, replace = FALSE)

qualitative_vars = c("key", "time_signature", "mode", "label")
songs = setQualitative(songs, qualitative_vars)

scaled = scale_data(songs, qualitative_vars)


classify = function(data) {
  # Encode as a one hot vector multilabel data
  songs.class <- cbind(data[1:10], class.ind(as.factor(data$key)), class.ind(as.factor(data$time_signature)), class.ind(as.factor(data$mode)),class.ind(as.factor(data$label)))
  keys = c("C", "C_major", "D_minor", "D", "D_major", "E_minor", "E", "E_major", "F_minor", "F", "F_major", "G_minor")
  # Set labels name
  names(songs.class) <- c(names(data)[1:10], keys, "oneforuth", "threefouths", "fourfourth", "fiveforth", "minor", "major", "dislike","like")
  return(songs.class)
}

songs.class = classify(scaled)

train <- songs.class[training_indices,]
test <- songs.class[-training_indices,]


answer = neural_network(train, test)

test.error <- mean(test[c("like", "dislike")] != answer[[1]])

save_old_nn = function(old_nn) {
  save(old_nn, file = "nn.data")
}

