rm(list = ls()) #clears workspace

source("files.R")
source("log_reg.R")

songs.train <- read.csv('training_data.csv', header = T)
songs.classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = list("label", "key", "mode", "time_signature")
songs.train = setQualitative(songs.train, qualitative_vars)
songs.classify = setQualitative(songs.classify, qualitative_vars)

f = label ~ key + danceability + energy + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + time_signature

preds = log_reg(songs.train, songs.classify, f)

labels = preds[[1]]
answers = preds[[2]]

txt = ""
for (answer in answers) {
  txt = paste(txt, sprintf("%d", answer), sep = "")
}
print(txt)



