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

save_old_nn = function(old_nn) {
  save(old_nn, file = "nn.data")
}

