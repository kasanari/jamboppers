rm(list = ls()) #clears workspace

source("files.R")
source("log_reg.R")
source("discriminant.R")

songs.train <- read.csv('training_data.csv', header = T)
songs.classify <- read.csv('songs_to_classify.csv', header = T)

qualitative_vars = list("label", "key", "mode", "time_signature")
songs.train = setQualitative(songs.train, qualitative_vars)
songs.classify = setQualitative(songs.classify, qualitative_vars)


f = label ~  danceability + energy + loudness + speechiness + acousticness + instrumentalness + liveness + valence 

preds = quadda(songs.train, songs.classify, f)

 txt <- makeWebsiteString(preds)
print(txt)
