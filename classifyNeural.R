rm(list = ls()) #clears workspace

##Neural network classification##
set.seed(1)
source("files.R")
library(neuralnet)
load("nn.data")
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


'%nin%' <- Negate('%in%')

labels_to_add = T_labels[T_labels %nin% C_labels]

classify.dummied[[labels_to_add]] = rep(0, n)

n <- labels(train.dummied)[[2]]
learning_vars = n[!n %in% c("(Intercept)", "labellike")]

weight_nn = nn[["weights"]]

#f <- as.formula(paste("labellike ~", paste(learning_vars, collapse = " + ")))

#nn <- neuralnet(f, data=train.dummied, hidden=c(17,10), linear.output=FALSE) #startweights = weight_nn)

answer = neural_network(train.dummied, classify.dummied)

#plot(nn)

##Predictions###

#t  = classify.dummied

#pr.nn <- compute(nn, t)

#answer = round(pr.nn[["net.result"]])

txt = ""
for (label in answer) {
  txt = paste(txt, sprintf("%d", label), sep = "")
}

print(txt)