rm(list = ls()) #clears workspace

##Neural network classification##
#set.seed(666)
source("files.R")
library(neuralnet)

source("neuralNetwork.R")

train <- read.csv('training_data.csv', header = T)
classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = c("key", "time_signature", "mode","label")
train = setQualitative(train, qualitative_vars)
classify = setQualitative(classify, qualitative_vars)

train = scale_data(train, qualitative_vars)
classify = scale_data(classify, qualitative_vars)


# Encode as a one hot vector multilabel data
train = cbind(train[1:10], class.ind(as.factor(train$key)), class.ind(as.factor(train$time_signature)), class.ind(as.factor(train$mode)),class.ind(as.factor(train$label)))
keys = c("C", "C_major", "D_minor", "D", "D_major", "E_minor", "E", "E_major", "F_minor", "F", "F_major", "G_minor")
# Set labels name
names(train) <- c(names(train)[1:10], keys, "onefourths", "threefourths", "fourfourths", "fivefourths", "minor", "major", "dislike","like")

classify = cbind(classify[1:10], class.ind(as.factor(classify$key)), class.ind(as.factor(classify$time_signature)), class.ind(as.factor(classify$mode)),class.ind(as.factor(classify$label)))
names(classify) <- c(names(classify)[1:10], keys, "onefourths", "threefourths", "fourfourths", "minor", "major")

fivefourths = rep(0, nrow(classify)) #Is missing from classify and is needed to make predictions
classify = cbind(classify, fivefourths)

load("old_weights.data")

#answer = neural_network(train, classify)

answers = matrix(nrow= 100,ncol= 100)
for (i in 1:5) {
  answer = neural_network(train, classify, old_weights)
  answers[,i] = answer[[1]][,1]
}

txt = ""
for (label in answers[,1]) {
  txt = paste(txt, sprintf("%d", label), sep = "")
}

print(txt)