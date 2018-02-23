rm(list = ls()) #clears workspace

##Neural network classification##
source("files.R")
library(neuralnet)

source("neuralNetwork.R")

load("old_weights.data")

load("classify_scaled_classed.data")
load("train_scaled_classed.data")

train <- final_train
classify <- final_classify

answer = neural_network(train, classify, old_weights)

txt = ""
for (label in answer[[1]]) {
  txt = paste(txt, sprintf("%d", label), sep = "")
}

print(txt)