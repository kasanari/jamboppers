rm(list = ls()) #clears workspace

set.seed(1)
source("files.R")
library(neuralnet)


neural_network = function(train, test) {
  ###Replace qualitative variables with dummy variables###
  #train_m = model.matrix(~., train)
  #test_m = model.matrix(~., test)
  
  ###Neural Network###
  n <- labels(train)[[2]]
  learning_vars = n[!n %in% c("(Intercept)", "labellike")]
  
  f <- as.formula(paste("labellike ~", paste(learning_vars, collapse = " + ")))
  
  nn <- neuralnet(f, data=train, hidden=c(17,10), linear.output=FALSE)
  
  plot(nn)
  
  ##Predictions###
  
  t  = test[, learning_vars]
  
  pr.nn <- compute(nn, t)
  
  answer = round(pr.nn[["net.result"]])
  
  answer.verbose <- rep("dislike", length(answer))
  answer.verbose[answer == 1] <- "like"
  
  return(answer)
}

scale_data = function(data, qualitative_vars) {
  data_to_scale = songs[, !(names(songs) %in% qualitative_vars)]
  maxs <- apply(data_to_scale, 2, max) 
  mins <- apply(data_to_scale, 2, min)
  
  scaled <- as.data.frame(scale(data_to_scale, center = mins, scale = maxs - mins))
  
  for (var in qualitative_vars) {
    scaled[[var]] = songs[[var]]
  }
  return(scaled)
}


songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 400, replace = FALSE)

qualitative_vars = c("label","key", "time_signature", "mode")
songs = setQualitative(songs, qualitative_vars)

scaled = scale_data(songs, qualitative_vars)

dummied = model.matrix(~., scaled)

train <- dummied[training_indices,]
test <- dummied[-training_indices,]

answer = neural_network(train, test)

test.error <- mean(test[,"labellike"] != answer)

