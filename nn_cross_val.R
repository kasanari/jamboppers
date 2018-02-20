#Cross validation algorithm modified from lab code

source("neuralNetwork.R")

#load("nn_old.data")
load("old_weights.data")

songs <- read.csv('training_data.csv', header = T)

qualitative_vars = c("key", "time_signature", "mode", "label")
songs = setQualitative(songs, qualitative_vars)

scaled = scale_data(songs, qualitative_vars)


N.repeat = 3
N.CV = 100


songs.class = classify(scaled)

randomize.indices <- sample(nrow(songs.class), size = nrow(songs.class), replace = FALSE)

songs.randomized <- songs.class[randomize.indices,]

error = c()
weights_vector = c()

#old_weights = nn_old


for (i in 1:N.CV) {
    
    start.index = (i - 1) * ceiling(nrow(songs) / N.CV) + 1
    end.index = min(i * ceiling(nrow(songs) / N.CV), nrow(songs))
    validation.indices <- seq(from = start.index, to = end.index, by = 1)
    train <- songs.randomized[validation.indices, ]
    validate <- songs.randomized[-validation.indices, ]
    
    answer = neural_network(train, validate, old_weights)
    
    error[i] <- mean(validate[c("like", "dislike")] != answer[[1]])
    weights_vector = cbind(weights_vector, answer[[2]])
}
  old_weights = weights_vector[,which.min(error)]


mean_error = mean(error)
mean_correct = 1-mean_error

#lowest = which.min(rowMeans(error.crossvalidation))

#pred.final <- knn(train = X, test = X, cl = y, k = lowest)

