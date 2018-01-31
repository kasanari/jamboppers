rm(list = ls()) #clears workspace

source("files.R")

set.seed(1)

#importFiles()



songs <- read.csv('training_data.csv', header = T)

training_indices <- sample(nrow(songs), size = 250, replace = FALSE)

qualitative_vars = list("label","key", "time_signature")
songs = setQualitative(songs, qualitative_vars)

songs.train = songs[training_indices,]
songs.test = songs[-training_indices,]

songs.train.scaled = scaleData(songs.train)

f = label ~ key + danceability + energy + key + loudness + mode + speechiness + acousticness + instrumentalness + liveness + valence + time_signature    

log_reg.fit = glm(formula = f, data = songs.train, family = "binomial")
log_reg.probs = predict(object = log_reg.fit, newdata = songs.test, type="response")

log_reg.pred <- rep("dislike", length(log_reg.probs))
log_reg.pred[log_reg.probs > 0.5] <- "like"

table(songs.test$label, log_reg.pred)
print(mean(log_reg.pred == songs.test$label))

#songs.classify <- read.csv('songs_to_classify.csv', header = T)