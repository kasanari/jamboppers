#Cross validation algorithm modified from lab code

source("neuralNetwork.R")

#load("nn_old.data")
#load("old_weights.data")

load("classify_train.data")

songs = final_train

N.repeat = 3
N.CV = 5

set.seed(666)

randomize.indices <- sample(nrow(songs), size = nrow(songs), replace = FALSE)

songs.randomized <- songs[randomize.indices,]

error = c()
weights_vector = c()

for (i in 1:N.CV) {
    
    start.index = (i - 1) * ceiling(nrow(songs) / N.CV) + 1
    end.index = min(i * ceiling(nrow(songs) / N.CV), nrow(songs))
    validation.indices <- seq(from = start.index, to = end.index, by = 1)
    train <- songs.randomized[-validation.indices, ]
    validate <- songs.randomized[validation.indices, ]
    
    answer = neural_network(train, validate, old_weights)
    
    error[i] <- mean(validate[c("like")] != answer[[1]])
    weights_vector = cbind(weights_vector, answer[[2]])
}

old_weights = weights_vector[,which.min(error)]

mean_error = mean(error)
mean_correct = 1-mean_error

