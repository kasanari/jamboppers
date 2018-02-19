rm(list = ls()) #clears workspace

##Neural network classification##
set.seed(1)
source("files.R")
library(neuralnet)

source("neuralNetwork.R")

songs.train <- read.csv('training_data.csv', header = T)
songs.classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = c("label","key", "time_signature", "mode")
songs.train = setQualitative(songs.train, qualitative_vars)
songs.classify = setQualitative(songs.classify, qualitative_vars)

train.scaled = scale_data(songs.train, qualitative_vars)
classify.scaled = scale_data(songs.classify, qualitative_vars)

n <- names(classify.scaled)
learning_vars_classify = n[n != "label"]

classify.scaled = classify.scaled[ , !(names(classify.scaled) %in% "label") ]

train.dummied = model.matrix(~., train.scaled)
classify.dummied = model.matrix(~., classify.scaled)

T_labels = labels(train.dummied)[[2]]
C_labels= labels(classify.dummied)[[2]]

time_signature5 = rep(0, nrow(classify.dummied)) #Is missing from classify and is needed to make predictions

classify.dummied = cbind(classify.dummied, time_signature5)

answer = neural_network(train.dummied, classify.dummied)

txt = ""
for (label in answer[[1]]) {
  txt = paste(txt, sprintf("%d", label), sep = "")
}

print(txt)