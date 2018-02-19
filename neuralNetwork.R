
#set.seed(1)
source("files.R")
library(neuralnet)
#load("nn.data")
#load("nn2.data")
#load("nn3.data")
#load("nn4.data")
#load("nn5.data")
#load("nn6.data")
#load("nn7.data")
load("nn9.data")

neural_network = function(train, test) {
  
  ###Neural Network###
  n <- labels(train)[[2]]
  learning_vars = n[!n %in% c("(Intercept)", "labellike")]
  
  f <- as.formula(paste("labellike ~", paste(learning_vars, collapse = " + ")))
  
  nn <- neuralnet(f, data=train, hidden=c(17,10), linear.output=FALSE, startweights = nn$weights)
  
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

qualitative_vars = c("label","key", "time_signature", "mode")
songs = setQualitative(songs, qualitative_vars)

scaled = scale_data(songs, qualitative_vars)

dummied = model.matrix(~., scaled)

train <- dummied[training_indices,]
test <- dummied[-training_indices,]

answer = neural_network(train, test)

test.error <- mean(test[,"labellike"] != answer[[1]])



