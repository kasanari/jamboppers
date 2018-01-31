
source("files.R")

#importFiles()

songs.train <- read.csv('training_data.csv', header=T)
songs.test <- read.csv('songs_to_classify.csv', header=T)


scaleData = function(data) {
  names = names(data)
  data.scaled = data
  for (name in names) {
    data.scaled[name] = scale(data[name])
  }
  return(data.scaled)
}


songs.train.scaled = scaleData(songs.train)
#songs.scaledTrain$danceability = scale(songs.train$danceability)
#songs.scaledTrain$energy = scale(songs.train$energy)
