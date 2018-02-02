rm(list = ls()) #clears workspace

source("files.R")
source("log_reg.R")

set.seed(1)

songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 400, replace = FALSE)

qualitative_vars = list("label","key", "time_signature", "mode")
songs = setQualitative(songs, qualitative_vars)

songs.train = songs[training_indices,]
songs.test = songs[-training_indices,]

songs.train.scaled = scaleData(songs.train)

f = label ~ key + danceability + energy + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + time_signature

preds = log_reg(songs.train, songs.test, f)

preds.labels = preds[[1]]
preds.answers = preds[[2]]

table(songs.test$label, preds.labels)
print(mean(preds.labels == songs.test$label))